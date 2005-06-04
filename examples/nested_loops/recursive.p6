use v6;

################################
# Recursive solution by iblech #
################################

my @loops = ([1..3], ['a'..'e'], ['foo', 'bar']);

sub outer(*@vals) {
    my &helper = -> @prev, @rest {
        if @rest {
            # We've still got a rest to interate over.
            # We add all items of @rest[0] to the new @prev
            # in our sub-helper, and use @rest[1...] as new
            # rest, i.e. all elements of @rest except the
            # first one.
            helper [ *@prev, $_ ], @rest[1...] for @rest[0];
        }
        else {
            # We don't have to recurse further, so we
            # simply "return" @prev.
            take @prev;
        }
    };

    # @prev: Empty array
    # @rest: @vals
    gather { helper [], @vals };
}
my @a = outer @loops;
say join "\n", @a;
