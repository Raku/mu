my sub foo {
  fail OutOfItemsException;
}
my sub bar {
  foo;
  return "not ok 1\n";
  CATCH {
    when OutOfItemsException {
      return "ok 1\n";
    }
    return "not ok 1 - caught exception, but couldnt identify it";
  }
}
$OUT.print("1..1\n");
my $a = bar;
$OUT.print($a.FETCH);
