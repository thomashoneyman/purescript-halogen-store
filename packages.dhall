let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220502/packages.dhall
        sha256:38d347aeba9fe6359c208abe87a5cecf1ffb14294f11ad19664ae35c59b6e29a

in  upstream
  with
    halogen-hooks =
      { repo = "https://github.com/thomashoneyman/purescript-halogen-hooks"
      , version = "v0.6.0"
      , dependencies =
          [ "aff"
          , "arrays"
          , "bifunctors"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "free"
          , "freeap"
          , "halogen"
          , "halogen-subscriptions"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "refs"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          , "unsafe-reference"
          , "web-dom"
          , "web-html"
          ]
      }
