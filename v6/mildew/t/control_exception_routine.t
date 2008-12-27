$OUT.print("1..2\n");
my sub foo($code) {
   $code.();
   $OUT.print("not ok 2 - inside foo\n");
}
my sub bar() {
   $OUT.print("ok 1\n");
   foo({
      my $e = ::ControlExceptionReturn.new();
      $e.routine = &?ROUTINE;
      $e.throw();
   });
   $OUT.print("not ok 2 - inside bar\n");
}
bar();
$OUT.print("ok 2 - outside bar\n");
