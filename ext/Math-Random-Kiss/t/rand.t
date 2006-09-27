use v6-alpha;

use Test;

plan 4 + 2*10 + 2;

use Math::Random::Kiss;

=pod 

Basic tests for the Math::Random::Kiss;

=cut

my $RAND = Math::Random::Kiss.new();

ok($RAND, 'new() Math::Random::Kiss');
ok($RAND ~~ Rand, 'Math::Random::Kiss does Rand');

ok($RAND.rand() >= 0, 'rand() returns numbers greater than or equal to 0');
ok($RAND.rand() < 1, 'rand() returns numbers less than 1');

for 1 .. 10 {
  ok $RAND.rand(10) >=  0, "rand(10) always returns numbers greater than or equal to 0 ($_)";
  ok $RAND.rand(10)  < 10, "rand(10) always returns numbers less than 10 ($_)";
}

ok($RAND.srand(1), 'srand(1) parses');

sub repeat_rand ($seed) {
	$RAND.srand($seed);
	for 1..99 { $RAND.rand(); }
	return $RAND.rand();
}

ok(repeat_rand(314159) == repeat_rand(314159),
    'srand() provides repeatability for rand()' );
