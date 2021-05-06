let conf = ../spago.dhall
in conf //
  { dependencies = conf.dependencies
  , sources = conf.sources # [ "example/**/*.purs" ]
  }
