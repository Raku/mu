knowhow Foo {
  has $!bar;
  has $.baz;
  method bar {
    $!bar;
  }
}
$OUT.print("1..2\n");
Foo.bar = "ok 1\n";
Foo.baz = "ok 2\n";
$OUT.print(Foo.bar.FETCH);
$OUT.print(Foo.baz.FETCH);
