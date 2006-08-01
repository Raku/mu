use v6-alpha;

use Test;
plan 1;

diag "Testing for subroutine parameter with sinature(Not Impl)...";
eval q{
	sub foo(Code:($a -> $b) &bar, Code:($a, $b -> $c) &baz) { }
	1;
}, "Declare subroutine parameters with sinature. (Compile-time)";
