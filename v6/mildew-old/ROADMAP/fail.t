my sub foo {
  fail;
}
my sub bar {
  foo;
  return "not ok 1\n";
  CATCH {
    return "ok 1\n";
  }
}
$OUT.print("1..1\n");
my $a = bar;
$OUT.print($a.FETCH);
