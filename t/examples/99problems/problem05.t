use v6;
use Test;
plan 2;

# P05 (*) Reverse a list.

is <a b c d e>.reverse, <e d c b a>, 
    'We should be able to reverse a list';
my @array = <a b c d e>;
is @array.reverse, <e d c b a>, '... and arrays';
