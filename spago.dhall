{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "seabug-contract"
, dependencies =
  [ "aeson"
  , "aeson-helpers"
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
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "random"
  , "spec"
  , "strings"
  , "text-encoding"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
