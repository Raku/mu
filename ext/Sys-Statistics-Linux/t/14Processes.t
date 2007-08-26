use v6-alpha;
use Test;
use Sys::Statistics::Linux::Processes;

plan 33;

my $lxs = Sys::Statistics::Linux::Processes.new(pids => 1);
$lxs.init;
sleep 1;
my %stats = $lxs.get;

for < ppid nlwp owner pgrp state session ttynr minflt cminflt mayflt cmayflt
      stime utime ttime cstime cutime prior nice sttime actime vsize nswap
      cnswap cpu size resident share trs drs lrs dtp cmd cmdline > -> $stat {
    ok %stats<1>{$stat}.defined, "$stat is defined";
}
