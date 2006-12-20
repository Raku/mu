use v6-alpha;
use Test;
plan 1;

# P18 (**) Extract a slice from a list.
# 
# Given two indices, I and K, the slice is the list containing the elements
# between the I'th and K'th element of the original list (both limits included).
# Start counting the elements with 1.
# 
# Example:
# * (slice '(a b c d e f g h i k) 3 7)
# (C D E F G)

my @array = <a b c d e f g h i j k>;
is @array.splice(3, 5), <d e f g h>, 'We should be able to splice lists';
