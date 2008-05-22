use v6;
use Test;
plan 1;

# P15 (**) Replicate the elements of a list a given number of times.
# 
# Example:
# * (repli '(a b c) 3)
# (A A A B B B C C C)

sub repli (@list, int $count) returns Array {
    return map { $_ xx $count }, @list;
}
is repli(<a b c>, 3), <a a a b b b c c c>,
    'We should be able to replicate array elements';
