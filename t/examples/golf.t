#!/usr/bin/pugs

use v6;
require Test;

=kwid

Test golf examples

This runs examples/golf/tsanta.p6 to test both mad golfer asavige's
original golf solutions and rg0now's improved solutions.

=cut

plan 10;

# XXX: in file paths, '/' works on Unix and Windows.
# For greater portability, we will need File::Spec or equivalent.

my $PUGS        = './pugs';
$PUGS           = 'pugs' if $?OS ~~ rx:perl5{MSWin32|msys|mingw};

# XXX: should make $outtmp unique (using $$ say).
# XXX: this $outtmp/slurp will go away when backticks supported.
# XXX: should also check to verify that nothing is written to stderr.
my $outtmp      = 'outgolf.tmp';
my $golfdir     = 'examples/golf';
my $tsanta      = "$golfdir/tsanta.p6";

my @progs       = ( 'head', 'tail', 'rev', 'mid', 'wc' );
my $mad_sol     = join(' ', @progs.map():{ $golfdir ~ '/'        ~ $_ ~ '.p6' });
my $rg0now_sol  = join(' ', @progs.map():{ $golfdir ~ '/rg0now-' ~ $_ ~ '.p6' });

for ($mad_sol, $rg0now_sol) -> $s {
    unlink($outtmp);
    my $exists = -f $outtmp;   # XXX: precedence bug in -f (?)
    ok( ! $exists, "file '$outtmp' does not exist" );
    my $cmd = "$PUGS $tsanta $PUGS $s >$outtmp";
    ok( system($cmd), "Run '$cmd'" );
    ok( -f $outtmp, "file '$outtmp' exists" );
    my @lines = slurp($outtmp);
    # cmp_ok( +@lines, '==', 28, "output contains 28 lines" );
    ok( +@lines == 28, "output contains 28 lines" );
    is( @lines.pop(), "Hooray, you passed.\n", 'last line is "Hooray, you passed."' );
}

END { defined($outtmp) and unlink($outtmp) }
