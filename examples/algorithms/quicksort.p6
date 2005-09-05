#!/usr/bin/pugs
use v6;

# Please remember to update t/examples/examples.t and rename
# examples/output/algorithms/quicksort if you rename/move this file.

multi quicksort ( ) { () }

multi quicksort ( *$x, *@xs ) {
    my @pre  := @xs.grep:{ $_ < $x };
    my @post := @xs.grep:{ $_ >= $x };
    return(@pre.quicksort, $x, @post.quicksort);
}

(1, 5, 2, 4, 3).quicksort.say;
