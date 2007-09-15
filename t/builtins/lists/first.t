use v6-alpha;
use Test;

# L<S29/"List"/"=item first">

=kwid

built-in "first" tests

=cut

plan 7;

my @list = (1 .. 10);

{
    my $result = first { ($_ % 2) }, @list;
    ok($result ~~ Item, "first() returns an Item");
    is($result, 1, "returned value by first() is correct");
}

{
    my $result = @list.first( { ($_ == 4)});
    ok($result ~~ Item, "method form of first returns an item");
    is($result, 4, "method form of first returns the expected item");
}

{
    my $result = @list.first():{ ($_ == 4) };
    ok($result ~~ Item, "first():<block> returns an Item");
    is($result, 4, "first() returned the expected value");
}

{
    dies_ok { @list.first( { ($_ == 11) } ) }, "first should fail if there is no first element";
}
