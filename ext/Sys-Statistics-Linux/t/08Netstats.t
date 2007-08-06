use v6-alpha;
use Test;
use Sys::Statistics::Linux::NetStats;

plan 18;

my $lxs = Sys::Statistics::Linux::NetStats.new;
$lxs.init;
sleep 1;
my %stats = $lxs.get;

for %stats.keys -> $k {
    for <rxbyt rxpcks rxerrs rxdrop rxfifo rxframe rxcompr rxmulti txbyt
         txpcks txerrs txdrop txfifo txcolls txcarr txcompr ttpcks ttbyt> -> $stat {

        ok %stats{$k}.defined, "$stat is defined";
    }
    last;
}
