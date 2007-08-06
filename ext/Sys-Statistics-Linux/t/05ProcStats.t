use v6-alpha;
use Test;
use Sys::Statistics::Linux::ProcStats;

plan 3;

my $lxs = Sys::Statistics::Linux::ProcStats.new;
$lxs.init;
sleep 1;
my %stats = $lxs.get;

for <new runqueue count> -> $stat {
    ok %stats{$stat}.defined, "$stat is defined";
}
