use v6;
use Test;
plan 2;

# P20 (*) Remove the K'th element from a list.
# 
# Example:
# * (remove-at '(a b c d) 2)
# (A C D)

my @array = <a b c d>;
is @array.splice(1,1), <b>, 
    'We should be able to remove elements from a list';
is @array, <a c d>, '... and have the correct list as the result';
