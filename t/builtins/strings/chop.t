use v6-alpha;
use Test;

# L<S29/Str/"=item chop">

plan 2;

my $str = "foo";
is(chop($str), "fo", "o removed");
is($str, "foo", "original string unchanged");
