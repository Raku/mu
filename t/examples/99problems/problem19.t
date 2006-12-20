use v6-alpha;
use Test;
plan 2;

# P19 (**) Rotate a list N places to the left.
# 
# Examples:
# * (rotate '(a b c d e f g h) 3)
# (D E F G H A B C)
# 
# * (rotate '(a b c d e f g h) -2)
# (G H A B C D E F)
# 
# Hint: Use the predefined functions length and append, as well as the result of
# problem P17.

sub rotate (int $times is copy, *@list is copy) returns Array {
    if $times < 0 {
        $times += @list.elems;
    }
    @list.push: @list.shift for 1 .. $times;
    return @list;
}
is rotate(3, <a b c d e f g h>), <d e f g h a b c>,
    'We should be able to rotate lists forwards';
is rotate(-2, <a b c d e f g h>), <g h a b c d e f>,
    '... and backwards';
