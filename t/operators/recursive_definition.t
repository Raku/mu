use v6-alpha;

use Test;

plan 2;

eval q{{
    sub postfix:<!>($arg) {
	    if ($arg == 0) { 1;}
	    else { ($arg-1)! * $arg;}
    }

    ok(5! == 120, "recursive factorial works");
}} or flunk("recursive factorial works", :todo<feature>);
is($!, undef, "recursive factorial parses", :todo<feature>);
