use v6-pugs;

module t::packages::Export_PackC {
  sub foo_packc () is export {
    1;
  }
}
