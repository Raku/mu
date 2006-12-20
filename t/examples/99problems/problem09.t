use v6-alpha;
use Test;
plan 2;

# P09 (**) Pack consecutive duplicates of list elements into sublists.
# 
# If a list contains repeated elements they should be placed in separate sublists.
# 
# Example:
# * (pack '(a a a a b c c a a d e e e e))
# ((A A A A) (B) (C C) (A A) (D) (E E E E))

sub pack (*@array is copy) returns Array {
    my (@list, @packed);
    while @array {
        @list.push(@array.shift) while !@list || @list[0] eq @array[0];
        @packed.push([@list]);
        @list = ();
    }
    return @packed;
}
is pack(<a a a a b c c a a d e e e e>),
    [ [<a a a a>], [<b>], [<c c>], [<a a>], [<d>], [<e e e e>] ],
    'We should be able to pack lists';

# From Larry, http://perlmonks.org/?node_id=590147
sub group (*@array is copy) {
    gather {
        while @array {
            take [ 
                gather {
                    my $h = shift @array;
                    take $h;
                    while @array and $h eq @array[0] {
                        take shift @array;
                    }
                }
            ];
        }
    }
}
is group(<a a a a b c c a a d e e e e>),
    [ [<a a a a>], [<b>], [<c c>], [<a a>], [<d>], [<e e e e>] ],
    '... even using gather/take';
