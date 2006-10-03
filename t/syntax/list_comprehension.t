use v6-alpha;

use Test;

plan 4;

#L<S02/Lists/"list() comprehension">
my @x = -1..10;
my @y = 4..12;
my @results =
    gather {
        for 0 <= list(@x) < all(@y) {
            .take;
        }
    };
my @expected_results = 0..3;
is @results.elems, @expected_results.elems,
    'comprehensions with chained ops in list context return correct number' ~
        'of elems';

#L<S02/Lists/"original ordering" "guaranteed" "preserved">
ok all(@results) eqv any(@expected_results),
    "comprehensions w/chained ops in list context return subset of C<list>";
cmp_ok @results, &infix:<eqv>, @expected_results,
    'comprehensions w/chained ops in list context preserve ordering';

@x = -1..10;
@y = 4..9;
@results =
    gather {
        for 0 <= list(@x) < any(@y) != list(@y) {
            .take;
        }
    };
@expected_results = 0..8;
cmp_ok @results, &infix:<eqv>, @expected_results,
    'comprehensions w/chained ops in list context use first C<list>';
