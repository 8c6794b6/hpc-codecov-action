{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "hpc-codecov-action"
, dependencies =
  [ "aff"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "github-actions-toolkit"
  , "maybe"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "prelude"
  , "spec"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
