#!/usr/bin/pugs

use v6;
require Test;

plan 16;

ok(2 + 2 == 4, '2 and 2 make 4');
is(2 + 2, 4, '2 and 2 make 4');
isa_ok([1, 2, 3], 'List');

todo_ok(2 + 2 == 5, '2 and 2 make 5');
todo_is(2 + 2, 5, '2 and 2 make 5');  
todo_isa_ok({'one' => 1}, 'Hash');

pass('This test passed');
#fail('This test failed');

skip('skip this test for now');

todo_fail('this fails, but might work soon');

diag('some misc comments and documentation');
  
cmp_ok('test', sub ($a, $b) { ?($a gt $b) }, 'me', '... testing gt on two strings');
todo_cmp_ok('test', sub ($a, $b) { ?($a lt $b) }, 'me', '... testing lt on two strings');

eval 'die'; # try to ruin $!

eval_ok('my $a = 1; $a', "eval_ok");

todo_eval_ok('die', "todo_eval_ok");

eval_is('my $a = 1; $a', 1, "eval_is");
#eval_is('die', 1, "eval_is");

todo_eval_is('my $b = 1; $b', 2, "todo_eval_is");
todo_eval_is('die', 3, "die in todo_eval_is");

1;

