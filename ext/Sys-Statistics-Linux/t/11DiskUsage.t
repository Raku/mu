use v6-alpha;
use Test;
use Sys::Statistics::Linux::DiskUsage;

plan 5;

my $lxs = Sys::Statistics::Linux::DiskUsage.new;
my %stats = $lxs.get;

for %stats.keys -> $k {
    for <total usage free usageper mountpoint> -> $stat {
        ok %stats{$k}{$stat}.defined, "$stat is defined";
    }
    last;
}
