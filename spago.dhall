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
  , "control"
  , "debug"
  , "effect"
  , "exceptions"
  , "http-methods"
  , "maybe"
  , "monad-logger"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "strings"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
