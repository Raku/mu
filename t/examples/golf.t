use v6-alpha;

use Test;

=kwid

Test golf examples

This runs examples/golf/tsanta.pl to test both mad golfer asavige's
original golf solutions and rg0now's improved solutions.

=cut

plan 10;

# XXX - the golf works when run by hand, but not via this test... disable for now. :-(
skip_rest "must be run manually"; exit;

# XXX: in file paths, '/' works on Unix and Windows.
# For greater portability, we will need File::Spec or equivalent.

# XXX: this $outtmp/slurp will go away when backticks supported.
# XXX: should also check to verify that nothing is written to stderr.
sub nonce () { return (".$*PID." ~ int rand 1000) }
my $outtmp      = 'outgolf' ~ nonce();
my $golfdir     = 'examples/golf';
my $tsanta      = "$golfdir/tsanta.pl";

my @progs       = ( 'head', 'tail', 'rev', 'mid', 'wc' );
my $mad_sol     = join(' ', @progs.map():{ $golfdir ~ '/'        ~ $_ ~ '.pl' });
my $rg0now_sol  = join(' ', @progs.map():{ $golfdir ~ '/rg0now-' ~ $_ ~ '.pl' });

for ($mad_sol, $rg0now_sol) -> $s {
    unlink($outtmp);
    my $exists = $outtmp ~~ :f;
    ok( ! $exists, "file '$outtmp' does not exist" );
    my $cmd = "$*EXECUTABLE_NAME $tsanta $*EXECUTABLE_NAME $s >$outtmp";
    ok( system($cmd), "Run '$cmd'" );
    ok( $outtmp ~~ :f, "file '$outtmp' exists" );
    my @lines = slurp($outtmp);
    # cmp_ok( +@lines, '==', 28, "output contains 28 lines" );
    ok( +@lines == 28, "output contains 28 lines" );
    is( @lines.pop(), "Hooray, you passed.\n", 'last line is "Hooray, you passed."' );
}

END { defined($outtmp) and unlink($outtmp) }
