#!/usr/bin/pugs

use v6;
require Test;

plan 12;

todo_ok(2 + 2 == 5, '2 and 2 make 5');
todo_is(2 + 2, 5, '2 and 2 make 5');
todo_isnt(2 + 2, 4, '2 and 2 does make 4 (but we dont want it to)');
todo_like("HelloWorld", rx:perl5{\s}, '... testing like()');
todo_unlike("Hello World", rx:perl5{\s}, '... testing todo_unlike()');
todo_fail('this fails, but might work soon');
todo_cmp_ok('test', sub ($a, $b) { ?($a lt $b) }, 'me', '... testing lt on two strings');
todo_eval_ok('die', "todo_eval_ok");
todo_eval_is('my $b = 1; $b', 2, "todo_eval_is");
todo_eval_is('die', 3, "die in todo_eval_is");
todo_dies_ok -> { return "Testing throws_ok" }, '... it todo_dies_ok';
todo_lives_ok -> { die "test" }, '... it todo_lives_ok';
#todo_throws_ok -> { die "Testing todo_throws_ok" }, 'Testing throws_ok', '... it todo_throws_ok with a Str';
#todo_throws_ok -> { die "Testing todo_throws_ok" }, rx:perl5:i/testing throws_ok/, '... it todo_throws_ok with a Rule';