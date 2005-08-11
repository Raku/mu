#!/usr/bin/pugs
#
# Please remember to update examples/output/vmethods/num if you change this
# file so its output will change.
#

use v6;

multi sub zeros ( Int $value: ) {
    return '0' x $value;
}

multi sub spaces ( Int $value: ) {
    return ' ' x $value;
}

multi sub fill_with ( Int $value: Int $string ) {
    return substr( $string ~ $value, -$string.chars );
}

say 7.fill_with(3.zeros);
say 7.fill_with(3.spaces);
