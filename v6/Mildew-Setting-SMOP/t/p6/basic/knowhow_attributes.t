knowhow Foo {
  has $!bar;
  has $.baz;
  method bar {
    $!bar;
  }
  method ble {
    $.baz;
  }
}
say "1..2";
Foo.bar = "ok 1";
Foo.baz = "ok 2";
say Foo.bar;
say Foo.ble;
