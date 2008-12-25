use v6;
use Test;
plan 2;

# P14 (*) Duplicate the elements of a list.
# 
# Example:
# * (dupli '(a b c c d))
# (A A B B C C C C D D)

is map({ $_ xx 2 }, <a b c c d>), <a a b b c c c c d d>,
    'We should be able to duplicate the elements of a list';

my @result;
eval '@result = (<a b c c d> ==> map { $_ xx 2 })';
#?pugs todo 'feed ops'
#?rakudo todo 'feed ops'
is @result, <a a b b c c c c d d>,
    'We should be able to duplicate the elements of a list';
