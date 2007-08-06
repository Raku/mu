use v6-alpha;
use Test;
use Sys::Statistics::Linux::SockStats;

plan 5;

my $lxs = Sys::Statistics::Linux::SockStats.new;
my %stats = $lxs.get;

for <used tcp udp raw ipfrag> -> $stat {
    ok %stats{$stat}.defined, "$stat is defined";
}
