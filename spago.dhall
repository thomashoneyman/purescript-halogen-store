{ name = "halogen-store"
, dependencies =
  [ "aff"
  , "distributive"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
