use v6-alpha;
use Test;
plan 2;

# P03 (*) Find the K'th element of a list.
# 
# The first element in the list is number 1.
# Example:
# * (element-at '(a b c d e) 3)
# C

is <a b c d e>[3], 'd', 'We should be able to index into lists';
my @array = <a b c d e>;
is @array[3], 'd', '... and arrays';
