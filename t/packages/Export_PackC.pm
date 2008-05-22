use v6;

module t::packages::Export_PackC {
  sub foo_packc () is export {
    1;
  }
}
