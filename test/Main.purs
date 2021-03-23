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
import Test.Spec (before_, beforeAll, describe, it)
import Test.Spec.Assertions (shouldReturn)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, runSpecT, defaultConfig)

-- transformers
import Control.Monad.Except (runExceptT, throwError)

-- Internal
import Main (BuildTool(..), workWith, getHpcCodecov)

main :: Effect Unit
main = do
  ts <- runSpecT myConfig [consoleReporter] do
    beforeAll getHpcCodecov' do

      -- describe "Getting hpc-codecov" do
      --   it "should download hpc-codecov executable binary" do
      --     et_file <- runExceptT getHpcCodecov
      --     case et_file of
      --       Right file -> exists file `shouldReturn` true
      --       Left err -> fail (message err)

      let gen_report_with build project inputs = do
            before_ (build project) $
              it "should generate JSON report" \exe -> do
                project_root <- liftEffect (resolve ["hs"] project)
                outs <- workWith (pure exe) (inputs project_root)
                exists outs.report `shouldReturn` true

      describe "Generate report for project1 with stack" do
        gen_report_with stackBuild "project1" $ \root ->
          { build_tool: Stack
          , build_tool_args: "--skip-msys"
          , verbose: true
          , test_suite: "project1-test"
          , project_root: root
          , excludes: []
          , out: "codecov-stack.json" }

      describe "Generate report for project1 with cabal-install" do
        gen_report_with cabalBuild "project1" $ \root ->
          { build_tool: CabalInstall
          , build_tool_args: ""
          , verbose: true
          , test_suite: "project1-test"
          , project_root: root
          , excludes: ["Main", "Paths_project1"]
          , out: "codecov-cabal.json" }

  launchAff_ ts

myConfig :: Config
myConfig = defaultConfig {timeout = Just (Milliseconds 60000.0)}

getHpcCodecov' :: Aff String
getHpcCodecov' = do
  et_path <- runExceptT getHpcCodecov
  case et_path of
    Right path -> pure path
    Left err -> throwError (error "Failed to download hpc-codecov executable")

stackBuild :: String -> Aff Unit
stackBuild = buildProject "stack --skip-msys"
               ["build", "--fast", "--test", "--coverage"]

cabalBuild :: String -> Aff Unit
cabalBuild = buildProject "cabal" ["test", "--enable-coverage"]

buildProject :: String -> Array String -> String -> Aff Unit
buildProject build_cmd build_args project_name = do
  et_ec <- runExceptT do
    project_root <- liftEffect (resolve ["hs"] project_name)
    let options = defaultExecOptions {cwd = Just project_root}
    exec { command: build_cmd
         , args: Just build_args
         , options: Just options }
  case et_ec of
    Right 0.0 -> pure unit
    Right ec -> throwError (error $ "non 0 ec: " <> show ec)
    Left err -> throwError err
