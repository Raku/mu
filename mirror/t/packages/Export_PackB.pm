module t::packages::Export_PackB {
  use t::packages::Export_PackA;

  sub does_export_work () {
    try { exported_foo() } == 42;
  }
}
