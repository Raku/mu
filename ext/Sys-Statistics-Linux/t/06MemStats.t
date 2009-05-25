use v6;
use Test;
use Sys::Statistics::Linux::MemStats;

plan 12;

my $lxs = Sys::Statistics::Linux::MemStats.new;
my %stats = $lxs.get;

for <memused memfree memusedper memtotal buffers cached realfree
     realfreeper swapused swapfree swapusedper swaptotal> -> $stat {

    ok %stats{$stat}.defined, "$stat is defined";
}
