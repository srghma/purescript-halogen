let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/repro/**/*.purs" ],
  dependencies = config.dependencies
}
