#!perl6
use v6;

multi sub quicksort ( ) { () }

multi sub quicksort ( *$x, *@xs ) {
    my @pre  = @xs.grep:{ $x > $_};
    my @post = @xs.grep:{ $x <= $_ };
#   my @post = @xs.grep:{ $_ >= $x }; # XXX FIXME
    (@pre.quicksort, $x, @post.quicksort);
}

(1, 5, 2, 4, 3).quicksort.say;
