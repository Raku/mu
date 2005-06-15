#!/usr/bin/pugs
#
# Please remember to update examples/output/vmethods/math if you change this
# file so its output will change.
#

use v6;

multi sub is_even (Int $value:) {
    $value % 2 == 1;
}

multi sub is_odd (Int $value:) {
    $value % 2 == 0;
}

say "5 is " ~ (5.is_even ?? 'even' :: 'odd');
say "8 is " ~ (8.is_odd  ?? 'odd' :: 'even');
