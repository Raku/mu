my sub return(|$capture) {
   my $e = ::ControlExceptionReturn.new();
   $e.capture = $capture;
   $e.routine = CALLER::<&?ROUTINE>;
   $e.throw;
}
$OUT.print("1..2\n");
my sub foo($code) {
   $code.();
   $OUT.print("not ok 2 - inside foo\n");
}
my sub bar() {
   $OUT.print("ok 1\n");
   foo({ return "ok 3 - value returned"; });
   $OUT.print("not ok 2 - inside bar\n");
}
my $a = bar();
$OUT.print("ok 2 - outside bar\n");
$OUT.print($a.FETCH);