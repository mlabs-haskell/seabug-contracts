{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20211116/packages.dhall sha256:7ba810597a275e43c83411d2ab0d4b3c54d0b551436f4b1632e9ff3eb62e327a

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "bigints"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "gen"
          , "identity"
          , "integers"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-fs-aff"
          , "node-path"
          , "nonempty"
          , "numbers"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "sequences"
          , "spec"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "69bd18c4a9cffdebc45c55d2448740721a91854c"
        }
      , aeson-helpers =
        { dependencies =
          [ "aff"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "contravariant"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "foreign-object"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "record"
          , "spec"
          , "spec-quickcheck"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-bridge-aeson-helpers.git"
        , version = "44d0dae060cf78babd4534320192b58c16a6f45b"
        }
      , sequences =
        { dependencies =
          [ "arrays"
          , "assert"
          , "console"
          , "effect"
          , "lazy"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "partial"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-laws"
          , "tuples"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/hdgarrood/purescript-sequences"
        , version = "v3.0.2"
        }
      , properties =
          { dependencies = ["prelude", "console"]
          , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
          , version = "v0.2.0"
          }
      , lattice =
          { dependencies = ["prelude", "console", "properties"]
          , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
          , version = "v0.3.0"
          }
      , mote =
          { dependencies = [ "these", "transformers", "arrays" ]
          , repo = "https://github.com/garyb/purescript-mote"
          , version = "v1.1.0"
          }
      , medea =
          { dependencies =
            [ "aff"
            , "argonaut"
            , "arrays"
            , "bifunctors"
            , "control"
            , "effect"
            , "either"
            , "enums"
            , "exceptions"
            , "foldable-traversable"
            , "foreign-object"
            , "free"
            , "integers"
            , "lists"
            , "maybe"
            , "mote"
            , "naturals"
            , "newtype"
            , "node-buffer"
            , "node-fs-aff"
            , "node-path"
            , "nonempty"
            , "ordered-collections"
            , "parsing"
            , "partial"
            , "prelude"
            , "psci-support"
            , "quickcheck"
            , "quickcheck-combinators"
            , "safely"
            , "spec"
            , "strings"
            , "these"
            , "transformers"
            , "typelevel"
            , "tuples"
            , "unicode"
            , "unordered-collections"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/juspay/medea-ps.git"
        , version = "8b215851959aa8bbf33e6708df6bd683c89d1a5a"
        }
      , cardano-transaction-lib =
          { dependencies =
            [ "aeson"
            , "aff"
            , "aff-promise"
            , "affjax"
            , "arraybuffer-types"
            , "arrays"
            , "bifunctors"
            , "bigints"
            , "checked-exceptions"
            , "console"
            , "const"
            , "control"
            , "debug"
            , "effect"
            , "either"
            , "encoding"
            , "enums"
            , "exceptions"
            , "foldable-traversable"
            , "foreign-object"
            , "identity"
            , "integers"
            , "js-date"
            , "lattice"
            , "lists"
            , "maybe"
            , "medea"
            , "monad-logger"
            , "mote"
            , "newtype"
            , "node-buffer"
            , "node-fs"
            , "node-fs-aff"
            , "nonempty"
            , "ordered-collections"
            , "partial"
            , "prelude"
            , "profunctor"
            , "profunctor-lenses"
            , "quickcheck"
            , "quickcheck-laws"
            , "rationals"
            , "record"
            , "refs"
            , "spec"
            , "strings"
            , "tailrec"
            , "these"
            , "transformers"
            , "tuples"
            , "typelevel"
            , "typelevel-prelude"
            , "uint"
            , "undefined"
            , "unfoldable"
            , "untagged-union"
            , "variant"
            ]
        , repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git"
          -- should be same rev as in flake.nix
          -- https://github.com/Plutonomicon/cardano-transaction-lib/pull/696
          -- PR: Return error if no utxo is specified for a tx input & Fix transaction inputs locking
          -- Commit is from our branch calum/metadata-invalid-char-fix which is based off this PR and
          -- includes a small fix to get the build working.
        , version = "8ff7b75d2048a9ea9d625f97d27176f59b09fb6a"
        }
      }
in upstream // additions
