use v6;

say "1..5";

if ('a' x 3 eq 'aaa')       { say "ok 1" } else { say "not ok 1" }
if ('ab' x 4 eq 'abababab') { say "ok 2" } else { say "not ok 2" }
if (1 x 5 eq '11111')       { say "ok 3" } else { say "not ok 3" }
if ('' x 6 eq '')           { say "ok 4" } else { say "not ok 4" }

my @foo = 'x' xx 10;
if (@foo[0] eq 'x') { say "ok 5" } else { say "not ok 5" }
if (@foo[9] eq 'x') { say "ok 6" } else { say "not ok 6" }
if (+@foo == 10)    { say "ok 7" } else { say "not ok 7" }
