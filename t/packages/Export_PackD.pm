use v6-pugs;

module t::packages::Export_PackD {
  sub this_gets_exported_lexically () is export {
    'moose!'
  }
}
