#!/usr/bin/pugs

use v6;
require Test;

plan(4);

# eval should evaluate the code in the lexical scope of eval's caller
sub make_eval_closure { my $a = 5; sub ($s) { eval $s } };
ok(make_eval_closure()('$a') == 5);

ok(eval('5') == 5);
my $foo = 1234;
ok(eval('$foo') == $foo);

# traps die?
ok(!eval('die; 1'));
