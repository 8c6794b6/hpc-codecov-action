module Test.Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- aff
import Effect.Aff (Aff, launchAff_, bracket)

-- datetimes
import Data.Time.Duration (Milliseconds(..))

-- either
import Data.Either (Either(..))

-- exceptions
import Effect.Exception (error)

-- foldable-traversable
import Data.Traversable (traverse_)

-- github-actions-toolkit
import GitHub.Actions.Exec (exec, defaultExecOptions)

-- maybe
import Data.Maybe (Maybe(..))

-- node-fs-aff
import Node.FS.Aff (exists, unlink)

-- node-path
import Node.Path (resolve)

-- node-process
import Node.Process (setEnv, unsetEnv)

-- spec
import Test.Spec (before_, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldReturn)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, runSpecT, defaultConfig)

-- strings
import Data.String (toUpper)

-- transformers
import Control.Monad.Except (runExceptT, throwError)

-- Internal
import Main (Inputs, getHpcCodecovMeta, mainAff, runAction)

main :: Effect Unit
main = do
  ts <- runSpecT myConfig [consoleReporter] $ beforeAll_ rmExeIfExist do
    let report build project inputs_for = do
          before_ (build project) $
            it "should generate Codecov report" do
              project_root <- liftEffect (resolve ["hs"] project)
              let inputs = inputs_for project_root
              withInputs inputs mainAff
              out <- liftEffect $ resolve [project_root] inputs.out
              exists out `shouldReturn` true

    describe "Generate report for project1 with stack" do
      report stackBuild "project1" $ \root ->
        { target: "stack:project1-test"
        , mix: ""
        , src: ""
        , excludes: ""
        , out: "codecov-stack.json"
        , root: root
        , build: ""
        , skip: ""
        , verbose: true }


    describe "Generate report for project1 with cabal-install" do
      report cabalBuild "project1" $ \root ->
        { target: "cabal:project1-test"
        , mix: ""
        , src: ""
        , excludes: "Main,Paths_project1"
        , out: "codecov-cabal.json"
        , root: root
        , build: ""
        , skip: ""
        , verbose: true }

  launchAff_ ts

myConfig :: Config
myConfig = defaultConfig {timeout = Just (Milliseconds 60000.0)}

rmExeIfExist :: Aff Unit
rmExeIfExist = do
  meta <- runAction getHpcCodecovMeta
  resolved <- liftEffect $ resolve [] meta.exe
  whenM (exists resolved) $ unlink resolved

withInputs :: forall a. Inputs -> Aff a -> Aff a
withInputs inputs act = bracket acquire release (\_ -> act)
  where
    acquire = trav (\{k,v} -> setEnv (github_key k) v)
    release _ = trav (\{k} -> unsetEnv k)

    trav = liftEffect <<< flip traverse_ ps

    ps =
      [ p "target" inputs.target
      , p "mix" inputs.mix
      , p "src" inputs.src
      , p "excludes" inputs.excludes
      , p "out" inputs.out
      , p "verbose" (show inputs.verbose)
      , p "root" inputs.root
      , p "build" inputs.build
      , p "skip" inputs.skip
      ]

    p k v = {k:k, v:v}

    -- See: @actions/core/lib/input.js
    github_key name = "INPUT_" <> toUpper name


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
