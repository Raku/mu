use v6;

say "1..6";

my @s = (1, 2, 3, 4, 5);

if (eval 'shift(@s)' == 1) { say "ok 1 # TODO shift" } else { say "not ok 1 # TODO shift" }
if (eval 'shift(@s)' == 2) { say "ok 2 # TODO split" } else { say "not ok 2 # TODO shift" }
if (eval 'shift(@s)' == 3) { say "ok 3 # TODO split" } else { say "not ok 3 # TODO shift" }
if (eval 'shift(@s)' == 4) { say "ok 4 # TODO split" } else { say "not ok 4 # TODO shift" }
if (eval '@s.shift'  == 5) { say "ok 5 # TODO split" } else { say "not ok 5 # TODO shift" }
if (eval 'defined(shift(@s))') { say "not ok 6 # TODO shift" } else { 
    say "ok 6 # TODO split" 
}
