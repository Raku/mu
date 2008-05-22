use v6;

use Test;

plan 2;

#L<S06/Operator overloading>
eval q{{
    sub postfix:<!>($arg) {
	    if ($arg == 0) { 1;}
	    else { ($arg-1)! * $arg;}
    }

    ok(5! == 120, "recursive factorial works");
}} or flunk("recursive factorial works");
is($!, undef, "recursive factorial parses");
