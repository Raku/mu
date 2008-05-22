use v6;
use Test;
plan 1;

# P11 (*) Modified run-length encoding.
# 
# Modify the result of problem P10 in such a way that if an element has no
# duplicates it is simply copied into the result list. Only elements with
# duplicates are transferred as (N E) lists.
# 
# Example:
# * (encode-modified '(a a a a b c c a a d e e e e))
# ((4 A) B (2 C) (2 A) D (4 E))

sub encode (*@list)returns Array {
    my $count = 1;
    my (@encoded, $previous, $x);
    
    for @list {
        $x = $_;
        if $x eq $previous {
            $count++;
            next;
        }
        if defined $previous {
            @encoded.push( 1 == $count ?? $previous !! [$count, $previous]);
            $count = 1;
        }
        $previous = $x;
    }
    @encoded.push([$count, $x]);
    return @encoded;
}
is encode(<a a a a b c c a a d e e e e>),
    [ [<4 a>], 'b', [<2 c>], [<2 a>], 'd', [<4 e>] ],
    'We should be able to run-length encode lists';
