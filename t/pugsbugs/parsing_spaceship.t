use v6;
use Test;

=kwid

The spaceship operator parses incorrectly in multiple ways

=cut

plan 2;

my %ball = map { $_ => 1; } 1..12;
eval_is(
    (%ball{12}) <=> (%ball{11}),
    0,
    'parens with spaceship parse incorrectly',
);

%ball{12} = 0.5;
is(%ball{12} <=> %ball{11}, -1, 'When spaceship terms are non-trivial numbers it parses incorrectly');
