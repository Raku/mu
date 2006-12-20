use v6-alpha;
use Test;
plan 1;

# P22 (*) Create a list containing all integers within a given range.
# 
# If first argument is smaller than second, produce a list in decreasing order.
# Example:
# * (range 4 9)
# (4 5 6 7 8 9)

is (4 .. 9), <4 5 6 7 8 9>, 'We should be able to create ranges';
