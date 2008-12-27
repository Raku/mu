module Return {
    sub return(|$capture) is export {
	my $e = ::ControlExceptionReturn.new();
	$e.capture = $capture;
	$e.routine = CALLER::<&?ROUTINE>;
	$e.throw;
    }
}
