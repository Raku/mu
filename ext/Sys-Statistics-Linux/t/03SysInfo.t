use v6-alpha;
use Test;
use Sys::Statistics::Linux::SysInfo;

plan 10;

my $lxs = Sys::Statistics::Linux::SysInfo.new;
my %stats = $lxs.get;

for <hostname domain kernel release version memtotal swaptotal countcpus uptime idletime> -> $stat {
    ok %stats{$stat}.defined, "$stat is defined";
}
