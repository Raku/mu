say "1..2";
knowhow ClassHOW {
  method add_method {
    say "ok 1 - method called.";
  }
  method dispatch {
    say "ok 2 - method dispatch.";
  }
}
class Foo {
  method bar {
  }
}
Foo.bar;