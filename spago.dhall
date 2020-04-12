{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "assert"
  , "avar"
  , "console"
  , "const"
  , "coroutines"
  , "dom-indexed"
  , "effect"
  , "foreign"
  , "fork"
  , "free"
  , "freeap"
  , "halogen-vdom"
  , "media-types"
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "profunctor"
  , "psci-support"
  , "random"
  , "transformers"
  , "unsafe-coerce"
  , "unsafe-reference"
  , "web-file"
  , "web-socket"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
