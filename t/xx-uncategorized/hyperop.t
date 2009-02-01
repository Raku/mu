use v6;

use Test;
plan 1;

my @a = 1..5;
#?pugs todo 'bug'
is(try({ [+] (@a»++ ) }), 20, "»++ works");
