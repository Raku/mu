use v6-alpha;

use Test;
plan 1;

# L<S03/"Smart matching"> # mentions the Code:syntax
# Could use another smart link reference!

diag "Testing for subroutine parameter with signature...";
eval_ok q{
	sub foo(Code:($a --> $b) &bar, Code:($a, $b --> $c) &baz) { }
	1;
}, 
"Declare subroutine parameters with signature. (Compile-time)", :todo<feature>;
