use v6;

say "1..6";

my @push;
eval 'push @foo, 42';
if (@push[0] == 42) { say "ok 1 # TODO push" } else { say "not ok 1 # TODO push" }
eval '@foo.push(24)';
if (@push[0] == 42) { say "ok 2 # TODO split" } else { say "not ok 2 # TODO push" }
if (@push[1] == 24) { say "ok 3 # TODO split" } else { say "not ok 3 # TODO push" }
eval 'push @foo, 1, 2, 3';
if (@push[2] == 1) { say "ok 4 # TODO split" } else { say "not ok 4 # TODO push" }
if (@push[3] == 2) { say "ok 5 # TODO split" } else { say "not ok 5 # TODO push" }
if (@push[4] == 3) { say "ok 6 # TODO split" } else { say "not ok 6 # TODO push" }
