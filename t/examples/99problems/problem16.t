use v6;
use Test;
plan 5;

# P16 (**) Drop every N'th element from a list.
# 
# Example:
# * (drop '(a b c d e f g h i k) 3)
# (A B D E G H K)

sub drop(@list, Int $nth) {
    return map { @list[$_] }, grep { ($_+1) % $nth }, 0 .. @list.elems - 1;
}
is drop(<a b c d e f g h i k>, 3), <a b d e g h k>,
    'We should be able to drop list elements';

sub drop2(@list, Int $nth) {
    return map { @list[$_] if ($_+1) % $nth }, ^@list;
}
is drop2(<a b c d e f g h i k>, 3), <a b d e g h k>,
    'We should be able to drop list elements based on if returning ()';

sub drop3(@list, Int $nth) {
    gather for ^@list {
	take @list[$_] if ($_+1) % $nth;
    }
}
is drop3(<a b c d e f g h i k>, 3), <a b d e g h k>,
    'We should be able to drop list elements using gather';

eval q{{
sub drop4(@list, Int $nth) {
    return (@list[$_] if ($_+1) % $nth) for ^@list;
}
is drop4(<a b c d e f g h i k>, 3), <a b d e g h k>,
    'We should be able to drop list elements using (statement if) for';
}} or flunk
    'We should be able to drop list elements using (statement if) for';

eval q{{
sub drop5(@list, Int $nth) {
    return @list[$_] if ($_+1) % $nth for ^@list;
}
is drop5(<a b c d e f g h i k>, 3), <a b d e g h k>,
    'We should be able to drop list elements using list comprehension';
}} or flunk
    'We should be able to drop list elements using list comprehension';

