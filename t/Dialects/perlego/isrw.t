use v6-alpha;

use Test;
exit;

plan 1;

skip_rest "This file was in t_disabled/.  Remove this SKIP of it now works.";
exit;

my $foo = 2;
eval '
    for $foo <-> $bar {
        $bar = 3;
    }
';
if ($foo == 3) { say "ok 1" } else { say "not ok 1" }

