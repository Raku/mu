use v6;
use Test;
plan 2;

# P08 (**) Eliminate consecutive duplicates of list elements.
# 
# 
# If a list contains repeated elements they should be replaced with a single
# copy of the element. The order of the elements should not be changed.
# 
# Example:
# * (compress '(a a a a b c c a a d e e e e))
# (A B C A D E)

# parens required in the assignment.  See http://perlmonks.org/?node=587242
my $compress = sub ($x) {
    state $previous;
    return $x ne $previous ?? ($previous = $x) !! ();
}
my @compressed = map $compress, <a a a a b c c a a d e e e e>;
is @compressed, <a b c a d e>, 'We should be able to compress lists';

multi compress2 () { () }
multi compress2 ($a) { item $a }
multi compress2 ($x, $y, *@xs) { $x xx ($x !=== $y), compress2($y, |@xs) }

my @x = <a a a a b c c a a d e e e e>;
is compress2(|@x), <a b c a d e>, '... even with multi subs';
