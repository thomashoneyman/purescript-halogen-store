{ name = "halogen-store"
, dependencies =
  [ "aff"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "refs"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
