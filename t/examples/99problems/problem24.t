use v6;
use Test;
plan 3;

# P24 (*) Lotto: Draw N different random numbers from the set 1..M.
# 
# The selected numbers shall be returned in a list.
# Example:
# * (lotto-select 6 49)
# (23 1 17 33 21 37)
# 
# Hint: Combine the solutions of problems P22 and P23.

# subset Positive::Int of Int where { $_ >= 0 };
# sub lotto (Positive::Int $count, Positive::Int $range) returns List {

sub lotto (int $count, int $range) returns List {
    return (1 .. $range).pick($count);
}

my @numbers = lotto(6, 49);
is @numbers.elems, 6, 'lotto() should return the correct number of numbers';
ok all(@numbers) ~~ any(1..49), '... and they should be in the correct range';

# currently (6.2.13 (r14927)) is turning {} into a hash composer at times it
# shouldn't, thus the clumsy syntax.
my %unique = map sub {$_ => 1}, @numbers;
is %unique.keys.elems, 6, '... and they should all be unique numbers';
