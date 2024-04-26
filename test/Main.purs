module Test.Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- aff
import Effect.Aff (Aff, launchAff_, bracket)

-- control
import Control.Alt ((<|>))

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

-- node-fs
import Node.FS.Stats (isFile)

-- node-fs-aff
import Node.FS.Aff (stat, unlink)

-- node-path
import Node.Path (resolve)

-- node-process
import Node.Process (setEnv, unsetEnv)

-- spec
import Test.Spec (SpecT, before_, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (Config, runSpecT, defaultConfig)

-- strings
import Data.String (toUpper)

-- transformers
import Control.Monad.Except (runExceptT, throwError)

-- Internal
import Main ( Inputs, Format(..), getHpcCodecovMeta, mainAff, runAction
            , defaultOutOnEmpty )

main :: Effect Unit
main = do
  aff <- runSpecT myConfig [consoleReporter] spec
  launchAff_ (void aff)

spec :: SpecT Aff Unit Effect Unit
spec = beforeAll_ rmExeIfExist do
  let report build project inputs_for =
        before_ (build project) $
          it "should generate Codecov report" do
            project_root <- liftEffect (resolve ["hs"] project)
            let inputs = inputs_for project_root
                oname = defaultOutOnEmpty inputs.format inputs.out
            withInputs inputs mainAff
            out <- liftEffect $ resolve [project_root] oname
            res <- stat out
            res `shouldSatisfy` isFile
      base_input =
        { target: ""
        , mix: ""
        , src: ""
        , excludes: "Main,Paths_project1"
        , skip: ""
        , format: Codecov
        , out: ""
        , root: ""
        , build: ""
        , expr_only: false
        , ignore_dittos: false
        , verbose: true
        }

  describe "Generate report for project1 with stack" do
    report stackBuild "project1" $ \root ->
      base_input { target = "stack:project1-test"
                 , out = "codecov-stack.json"
                 , expr_only = true
                 , ignore_dittos = true
                 , root = root }

  describe "Generate report for project1 with cabal-install" do
    report cabalBuild "project1" $ \root ->
      base_input { target = "cabal:project1-test"
                 , format = Lcov
                 , out = "codecov-cabal.lcov"
                 , root = root}

  describe "Generate report for project1 with cabal-install, cobertura" do
    report cabalBuild "project1" $ \root ->
      base_input { target = "cabal:project1-test"
                 , format = Cobertura
                 , out = "codecov-cabal.xml"
                 , root = root}

  describe "Generate report for project1 with cabal-install, default out" do
    report cabalBuild "project1" $ \root ->
      base_input { target = "cabal:project1-test"
                 , format = Lcov
                 , root = root }

myConfig :: Config
myConfig = defaultConfig {timeout = Just (Milliseconds 60000.0)}

rmExeIfExist :: Aff Unit
rmExeIfExist = do
  meta <- runAction getHpcCodecovMeta
  resolved <- liftEffect $ resolve [] meta.exe
  unlink resolved <|> pure unit

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
      , p "format" (show inputs.format)
      , p "verbose" (show inputs.verbose)
      , p "root" inputs.root
      , p "build" inputs.build
      , p "skip" inputs.skip
      , p "expr_only" (show inputs.expr_only)
      , p "ignore_dittos" (show inputs.ignore_dittos)
      ]

    p k v = {k:k, v:v}

    -- See: function "getInput" in @actions/core/lib/core.js
    github_key name = "INPUT_" <> toUpper name

stackBuild :: String -> Aff Unit
stackBuild = buildProject "stack"
               ["--skip-msys", "build", "--fast", "--test", "--coverage"]

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
