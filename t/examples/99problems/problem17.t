use v6;
use Test;
plan 2;

# P17 (*) Split a list into two parts; the length of the first part is given.
# 
# Do not use any predefined predicates.
# 
# Example:
# * (split '(a b c d e f g h i k) 3)
# ( (A B C) (D E F G H I K))

sub splitter ( @array is copy, int $length ) {
    my @head = @array.splice(0, $length);
    return (\@head, \@array);
}
my ( $a, $b ) = splitter(<a b c d e f g h i j k>, 3);
is $a, <a b c>,
    'The first array in the split should be correct';
is $b, <d e f g h i j k>, '... as should the second';
