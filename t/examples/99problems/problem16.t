use v6-alpha;
use Test;
plan 1;

# P16 (**) Drop every N'th element from a list.
# 
# Example:
# * (drop '(a b c d e f g h i k) 3)
# (A B D E G H K)

sub drop(@list, int $nth) {
    return map { @list[$_] }, grep { ($_+1) % $nth }, 0 .. @list.elems - 1;
}
is drop(<a b c d e f g h i k>, 3), <a b d e g h k>,
    'We should be able to drop list elements';
