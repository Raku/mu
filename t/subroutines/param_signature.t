use v6-alpha;

use Test;
plan 1;

diag "Testing for subroutine parameter with signature...";
eval_ok q{
	sub foo(Code:($a -> $b) &bar, Code:($a, $b -> $c) &baz) { }
	1;
}, 
"Declare subroutine parameters with signature. (Compile-time)", :todo<feature>;
