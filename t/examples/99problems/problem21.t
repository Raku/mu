use v6;
use Test;
plan 1;

# P21 (*) Insert an element at a given position into a list.
# 
# Example:
# * (insert-at 'alfa '(a b c d) 2)
# (A ALFA B C D)

my @array = <a b c d>;
@array.splice(1, 0, 'alfa');
is @array, <a alfa b c d>, 'We should be able to splice into an array';
