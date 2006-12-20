use v6-alpha;
use Test;
plan 1;

# P01 (*) Find the last box of a list.
# 
# Example:
# * (my-last '(a b c d))
# (D)

is <a b c d>.[-1], 'd', 'Find the last box of a list.';
