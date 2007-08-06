use v6-alpha;
use Test;
use Sys::Statistics::Linux::FileStats;

plan 10;

my $lxs = Sys::Statistics::Linux::FileStats.new;
my %stats = $lxs.get;

for <fhalloc fhfree fhmax inalloc infree inmax dentries unused agelimit wantpages> -> $stat {
    ok %stats{$stat}.defined, "$stat is defined";
}
