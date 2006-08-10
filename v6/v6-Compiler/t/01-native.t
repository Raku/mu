
use v6-alpha;

use v6::Grammar::Native;

say "1..2";
{
    my $s = '1';
    v6::Grammar::Native.num( $s );
    print "not " unless $/;
    say "ok 1";
}

{
    my $s = "'abc'";
    v6::Grammar::Native.str( $s );
    print "not " unless $/;
    say "ok 2";
}
