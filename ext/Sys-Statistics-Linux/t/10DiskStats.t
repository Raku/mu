use v6-alpha;
use Test;
use Sys::Statistics::Linux::DiskStats;

plan 8;

my $lxs = Sys::Statistics::Linux::DiskStats.new;
$lxs.init;
sleep 1;
my %stats = $lxs.get;

for %stats.keys -> $k {
    for <major minor rdreq rdbyt wrtreq wrtbyt ttreq ttbyt> -> $stat {
        ok %stats{$k}.defined, "$stat is defined";
    }
    last;
}
