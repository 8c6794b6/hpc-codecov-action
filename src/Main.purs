-- | GitHub action to generate Codecov test coverage report of
-- | cabalized Haskell package with `hpc-codecov`.

module Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- aff
import Effect.Aff (Aff, runAff_)

-- array
import Data.Array as Array

-- control
import Control.Alternative (guard)

-- either
import Data.Either (Either(..))

-- exceptions
import Effect.Exception (Error, error, message)

-- maybe
import Data.Maybe (Maybe(..), maybe)

-- node-buffer
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))

-- foldable-traversable
import Data.Foldable (foldM)
import Data.Traversable (traverse)

-- node-fs
import Node.FS.Perms (mkPerms, read, execute, all)
import Node.FS.Stats (isDirectory)

-- node-fs-aff
import Node.FS.Aff (chmod, exists, readdir, stat)

-- node-path
import Node.Path (resolve)

-- node-process
import Node.Platform (Platform(..))
import Node.Process (platform)

-- refs
import Effect.Ref as Ref

-- strings
import Data.String (Pattern(..), trim, contains, split)

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
  exe_exists <- lift $ exists meta.exe
  ifM (lift $ exists meta.exe)
    (do resolved <- liftEffect $ resolve [] meta.exe
        liftEffect $ Core.info ("Reusing " <> resolved)
        doWorkWith meta inputs (Just resolved))
    (doWorkWith meta inputs Nothing)

-- | The guts of guts of guts.
doWorkWith
  :: HpcCodecovMeta
  -- ^ Meta information to get `hpc-codecov`.
  -> Inputs
  -- ^ Input paramater object.
  -> Maybe String
  -- ^ 'Just' path to `hpc-codecov`, or 'Nothing' to download from
  -- latest release.
  -> Action Outputs
  -- ^ Output object of this action.
doWorkWith meta inputs mb_hpc_codecov = do
  hpc_codecov <- maybe (getHpcCodecov meta) pure mb_hpc_codecov
  hpc_codecov_args <- getHpcCodecovArgs inputs
  let options = Exec.defaultExecOptions {cwd = Just inputs.project_root}
  exec { command: hpc_codecov
       , args: Just hpc_codecov_args
       , options: Just options }
  report <- liftEffect $ resolve [inputs.project_root] inputs.out
  pure {exe: hpc_codecov, report: report}


-- ------------------------------------------------------------------------
--
-- Inputs and outputs
--
-- ------------------------------------------------------------------------

-- | Purescript representation of input information specified in
-- | `action.yml`.
type Inputs =
  { build_tool :: BuildTool
  , build_tool_args :: String
  , verbose :: Boolean
  , test_suite :: String
  , project_root :: String
  , excludes :: Array String
  , out :: String
  }

data BuildTool
  = CabalInstall
  | Stack

instance showBuildTool :: Show BuildTool where
  show CabalInstall = "cabal"
  show Stack = "stack"

-- | Purescript representation of output information specified in
-- | `action.yml`.
type Outputs =
  { exe :: String
  , report :: String
  }

-- | Get github action inputs specified in `action.yml`.
-- getInputs :: ExceptT Error Effect Inputs
getInputs :: Action Inputs
getInputs = liftEffect (runExceptT go) >>= except
  where
    go = do
      build_tool <- getBuildTool
      build_tool_args <- optionalInput "build-tool-args"
      out <- optionalInput "out"
      verbose <- getVerbose
      test_suite <- requiredInput "test-suite"
      project_root <- optionalInput "project-root"
      excludes <- getExcludes

      pure { build_tool: build_tool
           , build_tool_args: build_tool_args
           , verbose: verbose
           , test_suite: test_suite
           , project_root: project_root
           , excludes: excludes
           , out: out
           }

getBuildTool :: ExceptT Error Effect BuildTool
getBuildTool = do
  str <- requiredInput "build-tool"
  case str of
    "cabal" -> pure CabalInstall
    "stack" -> pure Stack
    _       -> throwError $ error $ "Unknown build-tool: " <> str

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

getExcludes :: ExceptT Error Effect (Array String)
getExcludes = do
  str <- optionalInput "excludes"
  pure $ split (Pattern ",") str

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
  "https://github.com/8c6794b6/hpc-codecov/releases/download/v0.2.0.2-rc5/"
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

-- | Get arguments passed to `hpc-codecov`.
getHpcCodecovArgs :: Inputs -> Action (Array String)
getHpcCodecovArgs inputs =
  case inputs.build_tool of
    Stack        -> getArgsForStack inputs
    CabalInstall -> getArgsForCabalInstall inputs

getArgsForStack :: Inputs -> Action (Array String)
getArgsForStack inputs = do
  let newRef = liftEffect <<< Ref.new
      readRef = liftEffect <<< Ref.read
      writeRef ref = liftEffect <<< flip Ref.write ref

  -- 'Ref String' to hold output from stack command.
  ref <- newRef ""

  let stack_cmd = case inputs.build_tool_args of
        "" -> "stack"
        args -> "stack " <> args
      options =
        Exec.defaultExecOptions { listeners = Just listeners
                                , cwd = Just inputs.project_root }
      listeners =
        Exec.defaultExecListeners {stdout = Just stdout_to_ref}
      stdout_to_ref buf = toString UTF8 buf >>= writeRef ref
      stack args = do
        exec { command: stack_cmd
             , args: Just args
             , options: Just options }
        readRef ref
      tix_name = inputs.test_suite <> ".tix"

  dist_dir <- stack ["path", "--dist-dir"]

  local_hpc_root <- stack ["path", "--local-hpc-root"]
  mb_tix <- findFileUnder (trim local_hpc_root) tix_name
  tix <- case mb_tix of
    Just t -> pure t
    Nothing -> throwError $ error ("cannot find tix: " <> tix_name)

  resolved_out <- liftEffect $ resolve [inputs.project_root] inputs.out

  pure $
    verboseArg inputs <>
    excludeArgs inputs <>
    [ "--mix", trim dist_dir <> "/hpc"
    , "--out", resolved_out, tix ]

getArgsForCabalInstall :: Inputs -> Action (Array String)
getArgsForCabalInstall inputs = do
  let distdir_name = "dist-newstyle"
  distdir <- liftEffect $ resolve [inputs.project_root] distdir_name

  let tix_name = inputs.test_suite <> ".tix"
  mb_tix <- findFileUnder distdir tix_name
  tix <- case mb_tix of
    Just t -> pure t
    Nothing -> throwError $ error ("cannot find tix: " <> tix_name)

  mixdirs <- findDirsUnder distdir "mix"
  let isVanilla = contains (Pattern "vanilla")
  vanilla_mix <- case Array.find isVanilla mixdirs of
    Just v -> pure v
    _ -> throwError $ error ("cannot find vanilla mix dir")
  vanilla_contents <- lift $ readdir vanilla_mix
  mix_args <- liftEffect $ traverse (resolve [vanilla_mix]) vanilla_contents

  resolved_out <- liftEffect $ resolve [inputs.project_root] inputs.out

  pure $
    verboseArg inputs <>
    excludeArgs inputs <>
    Array.concatMap (\d -> ["--mix", d]) mix_args <>
    [ "--out", resolved_out, tix ]

verboseArg :: Inputs -> Array String
verboseArg inputs = guard inputs.verbose *> pure "--verbose"

excludeArgs :: Inputs -> Array String
excludeArgs inputs = Array.concatMap (\m -> ["-x", m]) inputs.excludes


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

-- | Find file under given directory.
findFileUnder
  :: String -- ^ Directory searched under.
  -> String -- ^ File name to look for.
  -> Action (Maybe String)
findFileUnder dir0 file = go Nothing dir0
  where
    go :: Maybe String -> String -> Action (Maybe String)
    go mb_found dir =
      case mb_found of
        Just _ -> pure mb_found
        Nothing -> do
          s <- lift $ stat dir
          if isDirectory s
             then do
               target <- liftEffect $ resolve [dir] file
               does_exist <- lift $ exists target
               if does_exist
                 then pure (Just target)
                 else do
                   files <- lift $ readdir dir
                   files' <- liftEffect $ traverse (resolve [dir]) files
                   foldM go Nothing files'
             else pure Nothing

-- | Find directory under given directory.
findDirsUnder
  :: String -- ^ Directory searched under.
  -> String -- ^ Directory name to look for
  -> Action (Array String)
findDirsUnder dir0 target = go [] dir0
  where
    go :: Array String -> String -> Action (Array String)
    go founds dir = do
      s <- lift $ stat dir
      if not $ isDirectory s
        then pure founds
        else do
          resolved <- liftEffect $ resolve [dir] target
          does_exist <- lift $ exists resolved

          -- XXX: Slow, appending with Array ...
          let founds' = if does_exist
                          then [resolved] <> founds
                          else founds

          contents <- lift $ readdir dir
          contents' <- liftEffect $ traverse (resolve [dir]) contents
          foldM go founds' contents'
