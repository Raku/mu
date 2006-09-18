use v6-alpha;

use Test;
plan 7;

# L<S03/Cross operators/formed syntactically by placing>
eval_ok '<a b> X,X <c d>', 'cross metaoperator parses', :todo<feature>;

# L<S03/Cross operators/"string concatenating form is">
eval_is '<a b> X~X <1 2>', <a1 a2 b1 b2>,
    'cross-concat produces expected result', :todo<feature>;

# L<S03/Cross operators/desugars to something like>
eval_is '[~]«( <a b> X,X <1 2> )', <a1 a2 b1 b2>,
    'X,X works with hyperconcat', :todo<feature>;

# L<S03/Cross operators/list concatenating form when used like this>
my @result = eval '<a b> X,X 1,2 X,X <x y>';
is @result.elems, 8, 'chained cross-comma produces correct number of elements',
    :todo<feature>;
my @expected = (
    ['a', 1, 'x'],
    ['a', 1, 'y'],
    ['a', 2, 'x'],
    ['a', 2, 'y'],
    ['b', 1, 'x'],
    ['b', 1, 'y'],
    ['b', 2, 'x'],
    ['b', 2, 'y'],
);
is @result, @expected,
    'chained cross-comma produces correct results', :todo<feature>;

# L<S03/Cross operators/any existing non-mutating infix operator>
eval_is '(1,2 X*X 3,4)', (3,4,6,8), 'cross-product works', :todo<feature>;

# L<S03/Cross operators/underlying operator non-associating>
dies_ok '@result XcmpX @expected XcmpX <1 2>',
    'non-associating ops cannot be cross-ops';
