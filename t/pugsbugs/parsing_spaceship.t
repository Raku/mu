use v6;
use Test;

=kwid

The spaceship operator parses incorrectly in multiple ways

=cut

plan 5;

my %ball = map { $_ => 1; } 1..12;
is(
    (%ball{12}) <=> (%ball{11}),
    0,
    'parens with spaceship parse incorrectly',
);

%ball{12} = 0.5;
is(%ball{12} <=> %ball{11}, -1, 'When spaceship terms are non-trivial numbers it parses incorrectly');

my $result_1 = (%ball{10..12} <=> %ball{1..3});
my $result_2 = (%ball{11,12} <=> %ball{1,2});
my $result_3 = ([0] <=> [1]);

is($result_1, 0, 'When spaceship terms are non-trivial members it parses incorrectly'); 
is($result_2, 0, 'When spaceship terms are non-trivial members it parses incorrectly'); 
is($result_3, 0, 'When spaceship terms are non-trivial members it parses incorrectly'); 
