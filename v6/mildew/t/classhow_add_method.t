$OUT.print("1..2\n");
knowhow ClassHOW {
  method add_method {
    $OUT.print("ok 1 - method called.\n");
  }
  method dispatch {
    $OUT.print("ok 2 - method dispatch.\n");
  }
}
class Foo {
  method bar {
  }
}
Foo.bar;