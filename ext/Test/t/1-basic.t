#!/usr/bin/pugs

use v6;
require Test;

plan 43;

ok(2 + 2 == 4, '2 and 2 make 4');
ok(2 + 2 == 4, desc => '2 and 2 make 4');
ok(2 + 2 == 4, :desc('2 and 2 make 4'));

ok(2 + 2 == 5, '2 and 2 doesnt make 5', 1);
ok(2 + 2 == 5, desc => '2 and 2 doesnt make 5', todo => 1);
ok(2 + 2 == 5, :desc('2 and 2 doesnt make 5'), :todo(1));

todo_ok(2 + 2 == 5, '2 and 2 make 5');

is(2 + 2, 4, '2 and 2 make 4');
is(2 + 2, 4, desc => '2 and 2 make 4');
is(2 + 2, 4, :desc('2 and 2 make 4'));

is(2 + 2, 5, '2 and 2 doesnt make 5', 1);
is(2 + 2, 5, todo => 1, desc => '2 and 2 doesnt make 5');
is(2 + 2, 5, :todo(1), :desc('2 and 2 doesnt make 5'));

todo_is(2 + 2, 5, '2 and 2 make 5');

isnt(2 + 2, 5, '2 and 2 does not make 5');
isnt(2 + 2, 5, desc => '2 and 2 does not make 5');
isnt(2 + 2, 5, :desc('2 and 2 does not make 5'));

isnt(2 + 2, 4, '2 and 2 does make 4', :todo(1));
isnt(2 + 2, 4, desc => '2 and 2 does make 4', todo => 1);
isnt(2 + 2, 4, :desc('2 and 2 does make 4'), todo => 1);

todo_isnt(2 + 2, 4, '2 and 2 does make 4 (but we dont want it to)');

my @list = ( 1, 2, 3 );
isa_ok(@list, 'List');
isa_ok({ 'one' => 1 }, 'Hash');

like("Hello World", rx:perl5{\s}, '... testing like()');
todo_like("HelloWorld", rx:perl5{\s}, '... testing like()');
unlike("HelloWorld", rx:perl5{\s}, '... testing unlike()');
todo_unlike("Hello World", rx:perl5{\s}, '... testing todo_unlike()');

pass('This test passed');
#fail('This test failed');

#skip('skip this test for now');

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

use_ok('t::use_ok_test');
#eval_ok('it_worked()', '... use_ok worked and the export was successful');
todo_fail('... use_ok worked and the export was successful'); # unTODOme

# Need to do a test loading a package that is not there,
# and see that the load fails. Gracefully. :)
# fail_ok( use_ok('Non::Existent::Package') )

todo_fail("use_ok('t::no_module_here')"); # unTODOme
#todo_use_ok('t::no_module_here');

dies_ok -> { die "Testing dies_ok" }, '... it dies_ok';
todo_dies_ok -> { return "Testing throws_ok" }, '... it todo_dies_ok';

lives_ok -> { return "test" }, '... it lives_ok';
todo_lives_ok -> { die "test" }, '... it todo_lives_ok';

#throws_ok -> { die "Testing throws_ok" }, 'Testing throws_ok', '... it throws_ok with a Str';
#throws_ok -> { die "Testing throws_ok" }, rx:perl5:i/testing throws_ok/, '... it throws_ok with a Rule';

#todo_throws_ok -> { die "Testing todo_throws_ok" }, 'Testing throws_ok', '... it todo_throws_ok with a Str';
#todo_throws_ok -> { die "Testing todo_throws_ok" }, rx:perl5:i/testing throws_ok/, '... it todo_throws_ok with a Rule';

1;

