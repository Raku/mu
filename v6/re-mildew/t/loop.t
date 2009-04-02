sub return(|$capture) {
    my $e = ::ControlExceptionReturn.new();
    $e.capture = $capture;
    $e.routine = CALLER::<&?ROUTINE>;
    $e.throw;
}
sub foo {
   loop {
       say "ok";
       return;
   }
}
say "1..1";
foo();