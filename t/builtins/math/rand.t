use v6-alpha;

use Test;

plan tests => 2 + 2*10 + 2;

=pod 

Basic tests for the rand() builtin

=cut

# L<S29/"Math::Basic" /rand/>

ok(rand() >= 0, 'rand() returns numbers greater than or equal to 0');
ok(rand() < 1, 'rand() returns numbers less than 1');

for 1 .. 10 {
  ok rand(10) >=  0, "rand(10) always returns numbers greater than or equal to 0 ($_)";
  ok rand(10)  < 10, "rand(10) always returns numbers less than 10 ($_)";
}

# L<S29/"Math::Basic" /srand/>

eval_ok('srand(1)', 'srand(1) compiles', :todo);

sub repeat_rand ($seed) {
	srand($seed);
	for 1..99 { rand(); }
	return rand();
}

eval_ok( 'repeat_rand(314159) == repeat_rand(314159)',
    'srand() provides repeatability for rand()',
    :todo, :depends('srand()')
);
