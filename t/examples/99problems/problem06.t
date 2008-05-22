use v6;
use Test;
plan 2;

# P06 (*) Find out whether a list is a palindrome.
# 
# A palindrome can be read forward or backward; e.g. (x a m a x).

my @list = < a b c d e >;
isnt @list.reverse, @list, "<a b c d e> is not a palindrome";

@list = < a b c b a >;
is @list.reverse, @list, "<a b c b a> is a palindrome";
