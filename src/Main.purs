-- | GitHub action to generate Codecov test coverage report of
-- | cabalized Haskell package with `hpc-codecov`.

module Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- aff
import Effect.Aff (Aff, runAff_)

-- either
import Data.Either (Either(..))

-- exceptions
import Effect.Exception (Error, error, message)

-- maybe
import Data.Maybe (Maybe(..), maybe)

-- node-fs
import Node.FS.Perms (mkPerms, read, execute, all)
import Node.FS.Constants (x_OK)

-- node-fs-aff
import Node.FS.Aff (access', chmod)

-- node-path
import Node.Path (resolve)

-- node-process
import Node.Platform (Platform(..))
import Node.Process (platform)

-- transformers
import Control.Monad.Except (ExceptT, except, runExceptT, throwError, lift)

-- github-actions-toolkit
import GitHub.Actions.Core as Core
import GitHub.Actions.Exec as Exec


-- ------------------------------------------------------------------------
--
-- The main function
--
-- ------------------------------------------------------------------------

main :: Effect Unit
main = flip runAff_ mainAff $ \et_done ->
  case et_done of
    Right _ -> pure unit
    Left err -> Core.setFailed $ message err

mainAff :: Aff Unit
mainAff = runAction work


-- ------------------------------------------------------------------------
--
-- Internal work
--
-- ------------------------------------------------------------------------

type Action a = ExceptT Error Aff a

runAction :: forall a. Action a -> Aff a
runAction act = do
  et_ret <- runExceptT act
  case et_ret of
    Right ret -> pure ret
    Left err -> throwError err

-- | The guts.
work :: Action Unit
work = getInputs >>= doWork >>= setOutputs

-- | The guts of guts.
doWork :: Inputs -> Action Outputs
doWork inputs = do
  meta <- getHpcCodecovMeta
  resolved <- liftEffect $ resolve [] meta.exe
  mb_err <- lift $ access' resolved x_OK
  let {msg, mb_exe} = case mb_err of
        Just _ -> {msg: "Fetching " <> meta.url, mb_exe: Nothing}
        Nothing -> {msg: "Reusing " <> resolved, mb_exe: Just resolved}
  liftEffect (Core.info msg)
  doWorkWith meta inputs mb_exe

-- | The guts of guts of guts.
doWorkWith
  :: HpcCodecovMeta
  -- ^ Meta information to get `hpc-codecov`.
  -> Inputs
  -- ^ Input paramater object.
  -> Maybe String
  -- ^ 'Just' path to `hpc-codecov`, or 'Nothing' to download latest
  -- release from the github repository.
  -> Action Outputs
  -- ^ Output object of this action.
doWorkWith meta inputs mb_hpc_codecov = do
  hpc_codecov <- maybe (getHpcCodecov meta) pure mb_hpc_codecov
  args <- map Just $ getHpcCodecovArgs inputs
  let options = Just $ Exec.defaultExecOptions {cwd = Just inputs.root}
  exec {command: hpc_codecov, args, options}
  report <- liftEffect $ resolve [inputs.root] inputs.out
  pure {exe: hpc_codecov, report}


-- ------------------------------------------------------------------------
--
-- Inputs and outputs
--
-- ------------------------------------------------------------------------

-- | Purescript representation of input information specified in
-- | `action.yml`.
type Inputs =
  { target :: String
  , mix :: String
  , src :: String
  , excludes :: String
  , out :: String
  , format :: Format
  , root :: String
  , build :: String
  , skip :: String
  , verbose :: Boolean
  }

-- | Purescript representation of output information specified in
-- | `action.yml`.
type Outputs =
  { exe :: String
  , report :: String
  }

-- | Purescript representation of supported output report format.
data Format = Codecov | Lcov

instance Show Format where
  show Codecov = "codecov"
  show Lcov = "lcov"

-- | Get github action inputs specified in `action.yml`.
getInputs :: Action Inputs
getInputs = liftEffect (runExceptT go) >>= except
  where
    go = do
      target <- requiredInput "target"
      mix <- optionalInput "mix"
      src <- optionalInput "src"
      excludes <- optionalInput "excludes"
      format <- getFormat
      out <- defaultOutOnEmpty format <$> optionalInput "out"
      verbose <- getVerbose
      root <- optionalInput "root"
      build <- optionalInput "build"
      skip <- optionalInput "skip"

      pure {target, mix, src, excludes, out, format, verbose, root, build, skip}

getVerbose :: ExceptT Error Effect Boolean
getVerbose = do
  str <- optionalInput "verbose"
  case str of
    "true"  -> pure true
    "false" -> pure false
    _       -> do
      liftEffect $ Core.warning $
        "expecting 'true' or 'false' for input 'verbose', but got " <>
        show str <> ", setting verbosity to 'true'."
      pure true

getFormat :: ExceptT Error Effect Format
getFormat = do
  str <- optionalInput "format"
  case str of
    "codecov" -> pure Codecov
    "lcov" -> pure Lcov
    _ -> do
      liftEffect $ Core.error $
        "expecting 'codecov' or 'lcov' for input 'format', but got " <> show str
      throwError (error ("invalid format: " <> show str))

defaultOutOnEmpty :: Format -> String -> String
defaultOutOnEmpty format str =
  case format, str of
    Codecov, "" -> "codecov.json"
    Lcov, ""    -> "lcov.info"
    _, _        -> str

optionalInput :: String -> ExceptT Error Effect String
optionalInput n = Core.getInput {name: n, options: Nothing}

requiredInput :: String -> ExceptT Error Effect String
requiredInput n = Core.getInput {name: n, options: Just {required: true}}

-- | Set github action outputs on success, or set the action as failed
-- | with error message on failure.
setOutputs :: Outputs -> Action Unit
setOutputs outputs = liftEffect do
  Core.setOutput { name: "exe", value: outputs.exe }
  Core.setOutput { name: "report", value: outputs.report }


-- ------------------------------------------------------------------------
--
-- Fetching hpc-codecov
--
-- ------------------------------------------------------------------------

-- | Meta information to download and save `hpc-codecov` executable binary.
type HpcCodecovMeta =
  { url :: String
    -- ^ URL to download the binary executable.
  , exe :: String
    -- ^ Local executable path.
  }

-- | Make a URL to download `hpc-codecov`.
mkURL :: String -> String
mkURL name =
  "https://github.com/8c6794b6/hpc-codecov/releases/download/v0.4.1.0/"
  <> name

-- | Get meta information to download `hpc-codecov` executable for
-- | supported platforms. This action will fail on unsupported
-- | platforms, e.g. AIX, Android ... etc.
getHpcCodecovMeta :: Action HpcCodecovMeta
getHpcCodecovMeta = do
  case platform of
    Just Linux ->
      pure {url: mkURL "hpc-codecov-Linux", exe: "hpc-codecov"}
    Just Darwin ->
      pure {url: mkURL "hpc-codecov-macOS", exe: "hpc-codecov"}
    Just Win32 ->
      pure {url: mkURL "hpc-codecov-Windows.exe", exe: "hpc-codecov.exe"}
    _ ->
      throwError $ error $ "Unsupported platform: " <> show platform

-- | Download `hpc-codecov` executable. Internally uses `curl` to
-- | simplify following redirect of the download URL.  Returns the
-- | absolute path of the `hpc-codecov` executable.
getHpcCodecov :: HpcCodecovMeta -> Action String
getHpcCodecov meta = do
  exec { command: "curl"
       , args: Just ["-sL", "--output", meta.exe, meta.url]
       , options: Nothing }
  path <- liftEffect do
    resolved <- resolve [] meta.exe
    Core.info ("Saved hpc-codecov to: " <> resolved)
    pure resolved
  lift $ chmod path (mkPerms all (read + execute) (read + execute))
  pure path


-- ------------------------------------------------------------------------
--
-- Arguments for hpc-codecov
--
-- ------------------------------------------------------------------------

getHpcCodecovArgs :: Inputs -> Action (Array String)
getHpcCodecovArgs inputs = do
  let f opt str = if str == "" then [] else [opt, str]
  root_resolved <- liftEffect $ resolve [] inputs.root
  pure $
    [ inputs.target ] <>
    f "--mix" inputs.mix <>
    f "--src" inputs.src <>
    f "--exclude" inputs.excludes <>
    f "--out" inputs.out <>
    f "--format" (show inputs.format) <>
    f "--root" root_resolved <>
    f "--build" inputs.build <>
    f "--skip" inputs.skip <>
    if inputs.verbose then ["--verbose"] else []


-- ------------------------------------------------------------------------
--
-- Auxliary
--
-- ------------------------------------------------------------------------

-- | Execute and assert execution of external command.
exec :: Exec.ExecArgs -> Action Unit
exec args = do
  ec <- Exec.exec args
  when (ec /= 0.0) $
    throwError $ error $ "Got non zero exit code: " <> show ec
