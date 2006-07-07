use v6-alpha;

module t::packages::Export_PackC {
  sub foo_packc () is export {
    1;
  }
}
