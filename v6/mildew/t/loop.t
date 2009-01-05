sub return(|$capture) {
    my $e = ::ControlExceptionReturn.new();
    $e.capture = $capture;
    $e.routine = CALLER::<&?ROUTINE>;
    $e.throw;
}
sub foo {
   loop {
       $OUT.print("ok\n");
       return;
   }
}
$OUT.print("1..1\n");
foo();