use v6-alpha;
use Test;
plan 2;

# P04 (*) Find the number of elements of a list.

is <a b c d e>.elems, 5, 'We should be able to count the items in a list';
my @array = <a b c d e>;
is @array.elems, 5, '... and arrays';
