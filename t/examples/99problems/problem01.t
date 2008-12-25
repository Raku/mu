use v6;
use Test;
plan 2;

# P01 (*) Find the last box of a list.
# 
# Example:
# * (my-last '(a b c d))
# (D)

is <a b c d>.[*-1], 'd', 'Find the last box of a list.';

sub my_last (@xs) {
    return @xs[*-1];
}

is my_last(<a b c d>), 'd', 'Find the last box of a list via func.';
