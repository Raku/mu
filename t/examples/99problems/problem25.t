use v6;
use Test;
plan 1;

# P25 (*) Generate a random permutation of the elements of a list.
# 
# Example:
# * (rnd-permu '(a b c d e f))
# (B A D C E F)
# 
# Hint: Use the solution of problem P23.

my @array = ('a' .. 'f');
my @permute = @array.pick(*);
is @permute.sort, @array.sort,
    '.pick(*) should return a permutation of a list';
