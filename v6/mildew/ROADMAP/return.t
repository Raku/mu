my sub foo {
  {
    {
      return "ok 1\n";
    }
  }
  "not ok 1\n";
}
$OUT.print("1..1\n");
my $a = foo;
$OUT.print($a.FETCH);
