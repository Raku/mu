use v6;
use Test;
plan 4;

# P03 (*) Find the K'th element of a list.
# 
# The first element in the list is number 1.
# Example:
# * (element-at '(a b c d e) 3)
# C

is <a b c d e>[3], 'd', 'We should be able to index into lists';
my @array = <a b c d e>;
is @array[3], 'd', '... and arrays';

sub element_at (@xs, $pos) {
    return @xs[$pos];
}

is element_at(<a b c d e>, 3), 'd',
    'We should be able to index into lists by func';
is element_at(@array, 3), 'd', '... and arrays by func';
