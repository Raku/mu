use v6;

####################################
# Functional solution by blokhead  #
# Currently does not work but soon #
####################################

my @loops = ([1..3], ['a'..'e'], ['foo', 'bar']);

sub NestedLoops (*@loop) returns Ref {
    my @pos = 0 xx (@loop.elems - 1), -1;

    my sub incr($i) {
        if ( ++@pos[$i] == @loop[$i].elems ) {
            @pos[$i] = 0;
            return $i ?? incr($i - 1) :: 0;
        }
        return 1;
    };

    return sub {
       incr(@loop.end) or return;
       zip(@loop, @pos) ==> map -> $a, $i { $a[$i] };
    };
};

my $iter = NestedLoops(@loops);
my @group;

while @group = $iter() { say ~@group; }
