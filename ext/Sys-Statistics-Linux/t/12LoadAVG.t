use v6-alpha;
use Test;
use Sys::Statistics::Linux::LoadAVG;

plan 3;

my $lxs = Sys::Statistics::Linux::LoadAVG.new;
my %stats = $lxs.get;

for <avg_1 avg_5 avg_15> -> $stat {
    ok %stats{$stat}.defined, "$stat is defined";
}
