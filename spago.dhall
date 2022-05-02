{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies = ["aff", "cardano-transaction-lib", "identity"]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "src/**/*.purs", "test/**/*.purs" ]
}
