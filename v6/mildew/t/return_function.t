$OUT.print("1..5\n");
my sub return(|$capture) {
   my $e = ::ControlExceptionReturn.new();
   $e.capture = $capture;
   $e.routine = CALLER::<&?ROUTINE>;
   $e.throw;
}
my sub foo($code) {
   $code.();
   $OUT.print("not ok 2 - inside foo\n");
}
my sub bar() {
   $OUT.print("ok 1\n");
   foo({ return "ok 3 - value returned\n"; });
   $OUT.print("not ok 2 - inside bar\n");
}
my sub baz() {
   return "ok 4 - one item\n", "ok 5 - other item\n";
}
my $a = bar();
$OUT.print("ok 2 - outside bar\n");
$OUT.print($a.FETCH);
my $b = baz();
$OUT.print("# baz returned ");
$OUT.print($b.elems, "\n");
$OUT.print($b.shift);
$OUT.print($b.shift);
