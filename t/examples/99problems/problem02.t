use v6;
use Test;
plan 2;

# P02 (*) Find the last but one box of a list.
# 
# Example:
# * (my-but-last '(a b c d))
# (C D)

is <a b c d>[*-2, *-1], <c d>,
    'We should be able to grab the last two items from a list';

sub my_but_last (@xs) {
    return @xs[*-2,*-1];
}

is my_but_last(<a b c d>), <c d>,
    'We should be able to grab the last two items from a list by func';
