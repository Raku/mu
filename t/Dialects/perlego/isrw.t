use v6;

require Test;
exit;

plan 1;

my $foo = 2;
eval '
    for $foo <-> $bar {
        $bar = 3;
    }
';
if ($foo == 3) { say "ok 1" } else { say "not ok 1" }

