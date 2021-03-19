module Test.Main where

import Prelude

-- effect
import Effect (Effect)
import Effect.Class (liftEffect)

-- aff
import Effect.Aff (launchAff_)

-- node-fs
import Node.FS.Sync as FS

-- spec
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- Internal
import Main (getHpcCodecov)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Getting hpc-codecov" do
    it "should download hpc-codecov executable binary" do
      existence <- getHpcCodecov >>= liftEffect <<< FS.exists
      existence `shouldEqual` true
