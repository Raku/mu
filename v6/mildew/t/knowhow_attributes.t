knowhow Foo {
  has $!bar;
  method bar {
    $!bar;
  }
}
Foo.bar = "ok 1\n";
$OUT.print("1..1\n");
$OUT.print(Foo.bar.FETCH);
