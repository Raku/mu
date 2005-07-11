module t::pugsbugs::Export_PackB {
  use t::pugsbugs::Export_PackA;

  sub does_export_work () {
    try { exported_foo() } == 42;
  }
}
