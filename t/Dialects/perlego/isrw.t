use v6;

say "1..1";

my $foo = 2;
eval '
    for $foo <-> $bar {
        $bar = 3;
    }
';
if ($foo == 3) { say "ok 1" } else { say "not ok 1" }

