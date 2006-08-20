use v6-alpha;

use Test;

# Tests that (1|2|3) is the same as any(1,2,3).
# (Test primarily aimed at all backends which use PIL1 --
# (1|2|3) is treated as (1|(2|3)).)

plan 11;

is +any(1,2,3).values, 3;
is +(1 | 2 | 3).values, 3;

is +all(1,2,3).values, 3;
is +(1 & 2 & 3).values, 3;

is +one(1,2,3).values, 3;
is +(1 ^ 2 ^ 3).values, 3;

is +none(1,2,3).values, 3;

{
    #L<S09/Junctions/"used with chained operators">
    my @x = -1..10;
    my @y = 4..12;
    my @results =
        gather {
            for 0 <= any(@x) < all(@y) {
                .take;
            }
        };
    my @expected_results = 0..3;
    is @results.elems, @expected_results.elems,
        'junctions with chained ops in list context return correct number' ~
            'of elems';
    #L<S09/Junctions/"will be a subset">
    ok all(@results) eqv any(@expected_results),
        "junctions w/chained ops in list context return subset of C<any>";
    #L<S09/Junctions/"original ordering" "guaranteed" "preserved">
    ok @results eqv @expected_results,
        'junctions w/chained ops in list context preserve ordering';
    #L<<S09/Junctions/"subset of the first C<any>">>
    @x = -1..10;
    @y = 4..9;
    @results =
        gather {
            for 0 <= any(@x) < any(@y) {
                .take;
            }
        };
    @expected_results = 0..8;
    ok @results eqv @expected_results,
        'junctions w/chained ops in list context use first C<any>';
};
