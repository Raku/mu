
use v6-alpha;

use v6::Grammar::Native;

say "1..4";
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

{
    # non-matches
    my $s = 'abc';
    v6::Grammar::Native.num( $s );
    print "not " if $/;
    say "ok 3";

    v6::Grammar::Native.str( $s );
    print "not " if $/;
    say "ok 4";
}

