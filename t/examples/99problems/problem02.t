use v6-alpha;
use Test;
plan 1;

# P02 (*) Find the last but one box of a list.
# 
# Example:
# * (my-but-last '(a b c d))
# (C D)

is <a b c d>[-2, -1], <c d>,
    'We should be able to grab the last two items from a list';
