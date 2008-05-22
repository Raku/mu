use v6;
use Test;
plan 2;

# P07 (**) Flatten a nested list structure.
# 
# 
# Transform a list, possibly holding lists as elements into a `flat' list by
# replacing each list with its elements (recursively).
# 
# Example:
# * (my-flatten '(a (b (c d) e)))
# (A B C D E)
# 
# Hint: Use the predefined functions list and append.

my $flatten = -> $x { $x ~~ List ?? ( map $flatten, $x ) !! $x }; 
my @flattened = map $flatten, ('a', ['b', ['c', 'd', 'e']]);
is @flattened, <a b c d e>, 'We should be able to flatten lists';

sub my_flatten (@xs) {
    sub inner_flatten (*@xs) { return @xs; }

    return inner_flatten(@xs);
}

is my_flatten( ('a', ['b', ['c', 'd', 'e']]) ), <a b c d e>,
    'We should be able to flatten lists by func';
