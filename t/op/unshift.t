use v6;

say "1..6";

my @unshift;
eval 'unshift @foo, 42';
if (@unshift[0] == 42) { say "ok 1 # TODO unshift" } else { say "not ok 1 # TODO unshift" }
eval '@foo.unshift(24)';
if (@unshift[1] == 42) { say "ok 2 # TODO unshift" } else { say "not ok 2 # TODO unshift" }
if (@unshift[0] == 24) { say "ok 3 # TODO unshift" } else { say "not ok 3 # TODO unshift" }
eval 'unshift @foo, 1, 2, 3';
if (@unshift[0] == 1) { say "ok 4 # TODO unshift" } else { say "not ok 4 # TODO unshift" }
if (@unshift[1] == 2) { say "ok 5 # TODO unshift" } else { say "not ok 5 # TODO unshift" }
if (@unshift[2] == 3) { say "ok 6 # TODO unshift" } else { say "not ok 6 # TODO unshift" }
