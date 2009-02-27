module Foo {
  knowhow Bar {
    method baz {
      say 'ok 1';
    }
  }
}
say '1..1';
::Foo::Bar.baz
