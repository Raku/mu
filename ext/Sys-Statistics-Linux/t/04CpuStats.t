use v6-alpha;
use Test;
use Sys::Statistics::Linux::CpuStats;

plan 6; 

my $lxs = Sys::Statistics::Linux::CpuStats.new;
$lxs.init;
sleep 1;
my %stats = $lxs.get;

for <user nice system idle iowait total> -> $stat {
    ok %stats<cpu>{$stat}.defined, "$stat is defined";
}
