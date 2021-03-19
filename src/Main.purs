-- GitHub action to generate codecov report with hpc-codecov

module Main where

import Prelude

-- either
import Data.Either (Either(..))

-- maybe
import Data.Maybe (Maybe(..))

-- transformers
import Control.Monad.Except (runExceptT, throwError)

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- console
import Effect.Console (log, logShow)

-- exception
import Effect.Exception (Error, message)

-- aff
import Effect.Aff (Aff, runAff_)

-- github-actions-toolkit
import GitHub.Actions.Core as Core
import GitHub.Actions.Exec as Exec
import GitHub.Actions.IO as IO


-- ------------------------------------------------------------------------
--
-- The main function
--
-- ------------------------------------------------------------------------

main :: Effect Unit
main = do
  et_inputs <- getInputs
  case et_inputs of
    Right inputs -> runAff_ handler (work inputs)
    Left err -> Core.setFailed (message err)


-- ------------------------------------------------------------------------
--
-- Internals
--
-- ------------------------------------------------------------------------

work :: Inputs -> Aff Unit
work inputs = do
  et_path <- runExceptT $ IO.which {tool: "curl", check: Just true}
  case et_path of
    Right path -> liftEffect do
      log ("curl: " <> path)
      log ("build-tool: " <> inputs.build_tool)
      log ("build-tool-args: " <> inputs.build_tool_args)
    Left err -> throwError err

handler :: Either Error Unit -> Effect Unit
handler et_val = case et_val of
  Right _val -> Core.setOutput {name: "path", value: "NYI"}
  Left err   -> Core.setFailed (message err)

type Inputs =
  { build_tool :: String
  , build_tool_args :: String
  }

getInputs :: Effect (Either Error Inputs)
getInputs = runExceptT do
  build_tool <- Core.getInput {name: "build-tool", options: Just {required: true}}
  build_tool_args <- Core.getInput {name: "build-tool-args", options: Nothing}
  pure { build_tool: build_tool
       , build_tool_args: build_tool_args }

hpcCodecovURL :: String
hpcCodecovURL =
  "https://github.com/8c6794b6/hpc-codecov/releases/download/v0.2.0.2-rc5/hpc-codecov-Linux"

getHpcCodecov :: Aff String
getHpcCodecov = do
  et_ec <- runExceptT do
    Exec.exec { command: "curl"
              , args: Just ["-sL", "--output", "a.out", hpcCodecovURL]
              , options: Nothing }
  liftEffect $ case et_ec of
    Right ec -> logShow ec *> pure "a.out"
    Left err -> throwError err
