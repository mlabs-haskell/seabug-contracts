{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "cardano-transaction-lib"
  , "identity"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "control"
  , "effect"
  , "exceptions"
  , "foreign-object"
  , "http-methods"
  , "lists"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "transformers"
  , "tuples"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
