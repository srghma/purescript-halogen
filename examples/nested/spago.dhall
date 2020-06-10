let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/nested/**/*.purs" ],
  dependencies = config.dependencies
}
