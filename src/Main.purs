-- GitHub action to generate codecov report with hpc-codecov

module Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

-- aff
import Effect.Aff (Aff, runAff_)

-- console
-- import Effect.Console (log)

-- either
import Data.Either (Either(..))

-- exceptions
import Effect.Exception (Error, error, message)

-- maybe
import Data.Maybe (Maybe(..))

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
import Effect.Ref (Ref)
import Effect.Ref as Ref

-- strings
import Data.String (trim)

-- transformers
import Control.Monad.Except (ExceptT, runExceptT, throwError, lift)

-- github-actions-toolkit
import GitHub.Actions.Core as Core
import GitHub.Actions.Exec as Exec


-- ------------------------------------------------------------------------
--
-- The main function
--
-- ------------------------------------------------------------------------

main :: Effect Unit
main = do
  et_inputs <- runExceptT getInputs
  case et_inputs of
    Right inputs -> runAff_ setOutputs (work inputs)
    Left err     -> Core.setFailed (message err)


-- ------------------------------------------------------------------------
--
-- Internals
--
-- ------------------------------------------------------------------------

-- | The guts.
work :: Inputs -> Aff Outputs
work = workWith Nothing

-- | The guts of guts.
workWith
  :: Maybe String
  -- ^ 'Just' path to @hpc-codecov@, or 'Nothing' to download from
  -- latest release.
  -> Inputs
  -- ^ Input paramater object.
  -> Aff Outputs
  -- ^ Output object of this action.
workWith mb_hpc_codecov inputs = do
  et_outputs <- runAction do
    hpc_codecov <- case mb_hpc_codecov of
      Just path -> pure path
      Nothing -> getHpcCodecov
    hpc_codecov_args <- getHpcCodecovArgs inputs
    let options = Exec.defaultExecOptions {cwd = Just inputs.project_root}
    _ec <- Exec.exec { command: hpc_codecov
                     , args: Just hpc_codecov_args
                     , options: Just options }
    report <- liftEffect $ resolve [inputs.project_root] inputs.out
    pure { exe: hpc_codecov
         , report: report }

  case et_outputs of
    Right outputs -> pure outputs
    Left err -> throwError err


-- ------------------------------------------------------------------------
--
-- Inputs and outputs
--
-- ------------------------------------------------------------------------

-- | Purescript representation of input information specified in
-- @action.yml@.
type Inputs =
  { build_tool :: String
  , build_tool_args :: String
  , verbose :: Boolean
  , test_suite :: String
  , project_root :: String
  , out :: String
  }

-- | Purescript representation of output information specified in
-- @action.yml@.
type Outputs =
  { exe :: String
    -- ^ Absolute path to the @hpc-codecov@ executable.
  , report :: String
    -- ^ Absolute path to the generated coverage report JSON file.
  }

-- | Get github action @inputs@ specified in @action.yml@.
getInputs :: ExceptT Error Effect Inputs
getInputs = do
  let opt name = Core.getInput {name: name, options: Nothing}
      req name = Core.getInput {name: name, options: Just {required: true}}

  build_tool <- req "build-tool"
  build_tool_args <- opt "build-tool-args"
  out <- opt "out"
  verbose_str <- opt "verbose"
  verbose <- case verbose_str of
    "true" -> pure true
    "false" -> pure false
    _ -> do
      liftEffect $ Core.warning $
        "expecting 'true' or 'false' for input 'verbose', but got " <>
        show verbose_str <> ", setting verbosity to 'true'."
      pure true
  test_suite <- req "test-suite"
  project_root <- opt "project-root"

  pure { build_tool: build_tool
       , build_tool_args: build_tool_args
       , verbose: verbose
       , test_suite: test_suite
       , project_root: project_root
       , out: out
       }

-- | Set github action @outputs@ on success, or set the action as failed
-- with error message on failure.
setOutputs :: Either Error Outputs -> Effect Unit
setOutputs et_outputs = case et_outputs of
  Right outputs -> do
    Core.setOutput { name: "exe", value: outputs.exe }
    Core.setOutput { name: "report", value: outputs.report }
  Left err     -> Core.setFailed (message err)


-- ------------------------------------------------------------------------
--
-- Internal worker type
--
-- ------------------------------------------------------------------------

type Action a = ExceptT Error Aff a

runAction :: forall a. Action a -> Aff (Either Error a)
runAction = runExceptT


-- ------------------------------------------------------------------------
--
-- Fetching hpc-codecov
--
-- ------------------------------------------------------------------------

-- | Meta information to get @hpc-codecov@ executable binary.
type HpcCodecovMeta =
  { url :: String
    -- ^ URL to download the binary executable.
  , exe :: String
    -- ^ Local executable path.
  }

-- | Make URL to download @hpc-codecov@.
mkURL :: String -> String
mkURL name =
  "https://github.com/8c6794b6/hpc-codecov/releases/download/v0.2.0.2-rc5/"
  <> name

-- | Get meta information to get @hpc-codecov@ executable for
-- supported platforms. This action will fail on unsupported
-- platforms, e.g. AIX, Android ... etc.
getHpcCodecovMeta :: Action HpcCodecovMeta
getHpcCodecovMeta = do
  case platform of
    Just Linux ->
      pure {url: mkURL "hpc-codecov-Linux", exe: "hpc-codecov"}
    Just Darwin ->
      pure {url: mkURL "hpc-codecov-macOS", exe: "hpc-codecov"}
    Just Win32 ->
      pure {url: mkURL "hpc-codecov-Windows.exe", exe: "hpc-codecov.exe"}
    _ -> throwError (error "Unsupported platform")

-- | Download @hpc-codecov@ executable. Internally uses @curl@ to
-- simplify following redirect of the download URL.  Returns the
-- absolute path of the @hpc-codecov@ executable.
getHpcCodecov :: Action String
getHpcCodecov = do
  meta <- getHpcCodecovMeta
  ec <- Exec.exec { command: "curl"
                  , args: Just ["-sL", "--output", meta.exe, meta.url]
                  , options: Nothing }

  if ec == 0.0
     then do
       path <- liftEffect $ do
         resolved <- resolve [] meta.exe
         Core.info ("Saved hpc-codecov to: " <> resolved)
         pure resolved
       lift $ chmod path (mkPerms all (read + execute) (read + execute))
       pure path
    else
      throwError $ error $ "Downloading hpc-codecov failed with " <> show ec


-- ------------------------------------------------------------------------
--
-- Arguments for hpc-codecov
--
-- ------------------------------------------------------------------------

-- | Get arguments passed to @hpc-codecov@.
getHpcCodecovArgs :: Inputs -> Action (Array String)
getHpcCodecovArgs inputs =
  case inputs.build_tool of
    "stack" -> getArgsForStack inputs
    "cabal-install" -> getArgsForCabalInstall inputs
    other -> throwError (error ("unknown build tool: " <> other))

getArgsForStack :: Inputs -> Action (Array String)
getArgsForStack inputs = do
  -- 'Ref String' to hold output string from stack command.
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
        _ec <- Exec.exec { command: stack_cmd
                         , args: Just args
                         , options: Just options }
        readRef ref
      tix_file = inputs.test_suite <> ".tix"

  dist_dir <- stack ["path", "--dist-dir"]

  local_hpc_root <- stack ["path", "--local-hpc-root"]
  mb_tix <- findFileUnder (trim local_hpc_root) tix_file
  tix <- case mb_tix of
    Just t -> pure t
    Nothing -> throwError $ error ("cannot find tix: " <> inputs.test_suite)

  resolved_out <- liftEffect $ resolve [inputs.project_root] inputs.out

  pure [ "--verbose"
       , "--mix", trim dist_dir <> "/hpc"
       , "--out", resolved_out
       , tix ]

getArgsForCabalInstall :: Inputs -> Action (Array String)
getArgsForCabalInstall _ = throwError (error "Not Yet Implemented")


-- ------------------------------------------------------------------------
--
-- Auxliary
--
-- ------------------------------------------------------------------------

-- | Find file under given directory.
findFileUnder
  :: String -- ^ Directory to search
  -> String -- ^ File name to look for.
  -> Action (Maybe String)
findFileUnder dir0 file = go Nothing dir0
  where
    go :: Maybe String -> String -> Action (Maybe String)
    go mb_found dir =
      case mb_found of
        Just found -> pure (Just found)
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

newRef :: forall m a. MonadEffect m => a -> m (Ref a)
newRef = liftEffect <<< Ref.new

readRef :: forall m a. MonadEffect m => Ref a -> m a
readRef = liftEffect <<< Ref.read

writeRef :: forall m a. MonadEffect m => Ref a -> a -> m Unit
writeRef ref = liftEffect <<< flip Ref.write ref
