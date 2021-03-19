module Test.Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- aff
import Effect.Aff (Aff, launchAff_)

-- datetimes
import Data.Time.Duration (Milliseconds(..))

-- either
import Data.Either (Either(..))

-- exceptions
import Effect.Exception (error)

-- github-actions-toolkit
import GitHub.Actions.Exec (exec, defaultExecOptions)

-- maybe
import Data.Maybe (Maybe(..))

-- node-fs-aff
import Node.FS.Aff (exists)

-- node-path
import Node.Path (resolve)

-- spec
import Test.Spec (before_, describe, it)
import Test.Spec.Assertions (shouldReturn)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, runSpec', defaultConfig)

-- transformers
import Control.Monad.Except (runExceptT, throwError)

-- Internal
import Main (work)

myConfig :: Config
myConfig = defaultConfig {timeout = Just (Milliseconds 60000.0)}

main :: Effect Unit
main = launchAff_ $ runSpec' myConfig [consoleReporter] do

  -- describe "Getting hpc-codecov" do
  --   it "should download hpc-codecov executable binary" do
  --     et_file <- runExceptT getHpcCodecov
  --     case et_file of
  --       Right file -> exists file `shouldReturn` true
  --       Left err -> fail (message err)

  describe "Generate report for project1 with stack" do
    before_ (stackBuild "project1") $
      it "should generate JSON report" do
        project_root <- liftEffect (resolve ["hs"] "project1")
        let inputs = { build_tool: "stack"
                     , build_tool_args: "--skip-msys"
                     , verbose: true
                     , test_suite: "project1-test"
                     , project_root: project_root
                     , out: "codecov.json" }

        -- hpc_codecov <- liftEffect (resolve ["."] "hpc-codecov")
        -- outs <- workWith (pure hpc_codecov) inputs
        outs <- work inputs
        exists outs.report `shouldReturn` true

stackBuild :: String -> Aff Unit
stackBuild project_name = do
  et_ec <- runExceptT do
    project_root <- liftEffect (resolve ["hs"] project_name)
    let options = defaultExecOptions {cwd = Just project_root}
    exec { command: "stack --skip-msys"
         , args: Just ["build", "--fast", "--test", "--coverage"]
         , options: Just options }
  case et_ec of
    Right 0.0 -> pure unit
    Right _ec  -> throwError (error "error: non 0 ec")
    Left err -> throwError err
