
use v6-alpha;

use v6::Grammar::Native;

say "1..1";
{
    my $s = '1';
    v6::Grammar::Native.num( $s );
    print "not " unless $/;
    say "ok";
}
