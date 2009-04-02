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
Foo.bar = "ok 1\n";
Foo.baz = "ok 2\n";
say Foo.bar;
say Foo.ble;
