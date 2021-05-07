let conf = ../spago.dhall
in conf //
  { dependencies = conf.dependencies #
      [ "arrays"
      , "const"
      , "strings"
      , "tuples"
      , "variant"
      , "web-events"
      ]
  , sources = conf.sources #
      [ "example/**/*.purs"
      ]
  }
