{ name = "halogen-store"
, dependencies =
  [ "aff"
  , "distributive"
  , "effect"
  , "fork"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
