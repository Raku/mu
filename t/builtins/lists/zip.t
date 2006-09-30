use v6-alpha;

use Test;

=pod

The zip() builtin and operator tests

L<S03/"Traversing arrays in parallel">
L<S29/Container/"=item zip">

=cut

plan 12;

{
    my @a = (0, 2, 4);
    my @b = (1, 3, 5);

    my @e = (0 .. 5);

    my @z; @z = zip(@a; @b);
    my @y; @y = (@a ¥ @b);
    my @x; @x = (@a Y @b);

    is(~@z, ~@e, "simple zip");
    is(~@y, ~@e, "also with yen char");
    is(~@x, ~@e, "also with Y char");
};

{
    my @a = (0, 3);
    my @b = (1, 4);
    my @c = (2, 5);

    my @e = (0 .. 5);

    my @z; @z = zip(@a; @b; @c);
    my @y; @y = (@a ¥ @b ¥ @c);
    my @x; @x = (@a Y @b Y @c);

    is(~@z, ~@e, "zip of 3 arrays");
    is(~@y, ~@e, "also with yen char");
    is(~@x, ~@e, "also with Y char");
};

{
    my @a = (0, 4);
    my @b = (2, 6);
    my @c = (1, 3, 5, 7);

    # [((0, 2), 1), ((4, 6), 3), (undef, 5), (undef, 7)]
    my $todo = 'Seq(Seq(0,2),1), Seq(Seq(0,2),1), Seq(undef,5), Seq(undef,7)';
    my @e = eval $todo;

    my @z; @z = zip(zip(@a; @b); @c);
    my @y; @y = ((@a ¥ @b) ¥ @c);
    my @x; @x = ((@a Y @b) Y @c);

    is(~@z, ~@e, "zip of zipped arrays with other array", :todo<feature>,
        :depends<Seq>);
    is(~@y, ~@e, "also as ¥", :todo<feature>, :depends<Seq>);
    is(~@x, ~@e, "also as Y", :todo<feature>, :depends<Seq>);
};

{
    my @a = (0, 2);
    my @b = (1, 3, 5);
    my @e = (0, 1, 2, 3, undef, 5);

    my @z = (@a ¥ @b);
    is(@z, @e, "zip uses length of longest");
}

{
    my @a;
    my @b;

    (@a ¥ @b) = (1, 2, 3, 4);
    # XXX - The arrays below are most likely Seq's
    is(@a, [1, 3], "first half of two zipped arrays as lvalues", :todo);
    is(@b, [2, 4], "second half of the lvalue zip", :todo);
}
