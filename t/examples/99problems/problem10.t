use v6;
use Test;
plan 1;

# P10 (*) Run-length encoding of a list.
# 
# 
# Use the result of problem P09 to implement the so-called run-length encoding
# data compression method. Consecutive duplicates of elements are encoded as
# lists (N E) where N is the number of duplicates of the element E.
# 
# Example:
# * (encode '(a a a a b c c a a d e e e e))
# ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

sub encode (*@list) {
    my $count = 1;
    my ( @encoded, $previous, $x );
    
    for @list {
        $x = $_;
        if $x eq $previous {
            $count++;
            next;
        }
        if defined $previous {
            @encoded.push([$count, $previous]);
            $count = 1;
        }
        $previous = $x;
    }
    @encoded.push([$count, $x]);
    return @encoded;
}

is encode(<a a a a b c c a a d e e e e>),
    [ [<4 a>], [<1 b>], [<2 c>], [<2 a>], [<1 d>], [<4 e>] ],
    'We should be able to run-length encode lists';
