use v6;

say "1..6";

my @pop = (1, 2, 3, 4, 5);

if (eval 'pop(@pop)' == 5) { say "ok 1 # TODO pop" } else { say "not ok 1 # TODO pop" }
if (eval 'pop(@pop)' == 4) { say "ok 2 # TODO pop" } else { say "not ok 2 # TODO pop" }
if (eval 'pop(@pop)' == 3) { say "ok 3 # TODO pop" } else { say "not ok 3 # TODO pop" }
if (eval 'pop(@pop)' == 2) { say "ok 4 # TODO pop" } else { say "not ok 4 # TODO pop" }
if (eval '@pop.pop'  == 1) { say "ok 5 # TODO pop" } else { say "not ok 5 # TODO pop" }
if (eval 'defined(pop(@pop))') { say "not ok 6 # TODO pop" } else { 
    say "ok 6 # TODO pop" 
}
