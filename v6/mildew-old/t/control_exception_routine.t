say "1..2";
my sub foo($code) {
   $code();
   say "not ok 2 - inside foo";
}
my sub bar() {
   say "ok 1";
   foo({
      my $e = ::ControlExceptionReturn.new();
      $e.routine = &?ROUTINE;
      $e.throw();
   });
   say "not ok 2 - inside bar";
}
bar();
say "ok 2 - outside bar";
