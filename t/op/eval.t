use v6;

say "1..4";

# eval should evaluate the code in the lexical scope of eval's caller
sub make_eval_closure { my $a = 5; sub ($s) { eval $s } };
if (make_eval_closure()('$a') == 5) { say "ok 1" } else { say "not ok 1" }

if (eval('5') == 5) { say "ok 2" } else { say "not ok 2" }
my $foo = 1234;
if (eval('$foo') == $foo) { say "ok 3" } else { say "not ok 3" }

# traps die?
if (eval('die; 1')) { say "not ok 4" } else { say "ok 4" }
