{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "cardano-transaction-lib"
  , "const"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "http-methods"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "newtype"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "random"
  , "spec"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
