use v6-alpha;
use Test;
use Sys::Statistics::Linux::PgSwStats;

plan 4;

my $lxs = Sys::Statistics::Linux::PgSwStats.new;
sleep 1;
my %stats = $lxs.get;

for <pgpgin pgpgout pswpin pswpout> -> $stat {
    ok %stats{$stat}.defined, "$stat is defined";
}
