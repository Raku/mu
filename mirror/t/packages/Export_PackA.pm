module t::packages::Export_PackA {
  sub exported_foo () is export {
    42;
  }
}
