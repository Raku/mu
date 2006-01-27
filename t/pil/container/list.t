#!./pugs

use v6;
use Test::PIL::Bootstrap;

check_pil();

pil_is_eq(
    '^List.identifier()', 
    '"List-0.0.1-url:pugscode.org"', 
    '... List has the correct identifier');

pil_is_eq('^List.has_method("FETCH")', 'true', '... ^List.has_method(FETCH)');
pil_is_eq('^List.has_method("FETCH_LIST")', 'true', '... ^List.has_method(FETCH_LIST)');

pil_is_eq('^List.is_a(^Object)', 'true', '... ^List.is_a(^Object)');
pil_is_eq('^List.isa("Object")', 'true', '... ^List.isa(Object)');
pil_is_eq('^List.does("List")', 'true', '... ^List.does(List)');

for (qw(
    elems 
    join map grep reduce zip reverse sort
    pop push shift unshift
    )) -> $method_name {
    pil_is_eq(
        '^List.has_method("' ~ $method_name ~ '")', 
        'true', 
        '... ^List.has_method(' ~ $method_name ~ ')');
}

my $prelude = q:to/PRELUDE/
@l := ^List.new([ 1, 2, 3 ]);
PRELUDE;

pil_is_eq($prelude ~
    '@l.elems()',
    '3',
    '... @l.elems == 3');

pil_is_eq($prelude ~
    '@l.join(", ")',
    '"1, 2, 3"',
    '... @l.join(", ") == 1, 2, 3');    

pil_is_eq($prelude ~
    '[ @l.pop(), @l.pop(), @l.pop() ]',
    '[3, 3, 3]',
    '... @l.pop(), @l.pop(), @l.pop() == [3, 3, 3] # Lists are immutable');  

pil_is_eq($prelude ~
    '@l.push(4).FETCH_LIST()',
    '[1, 2, 3, 4]',
    '... @l.push(4) == [1, 2, 3, 4]');            

pil_is_eq($prelude ~
    '[ @l.shift(), @l.shift(), @l.shift() ]',
    '[1, 1, 1]',
    '... @l.shift(), @l.shift(), @l.shift() == [1, 1, 1] # Lists are immutable');            

pil_is_eq($prelude ~
    '@l.unshift(0).FETCH_LIST()',
    '[0, 1, 2, 3]',
    '... @l.unshift(0) == [0, 1, 2, 3]');    

pil_is_eq($prelude ~
    '@l.reverse().FETCH_LIST()',
    '[3, 2, 1]',
    '... @l.reverse() == [3, 2, 1]');  

pil_is_eq($prelude ~
    '@l.map(-> $x { $x`add(2) }).FETCH_LIST()',
    '[3, 4, 5]',
    '... @l.map(-> $x { $x + 2 }) == [3, 4, 5]');      

pil_is_eq($prelude ~
    '@l.grep(-> $x { $x`le(2) }).FETCH_LIST()',
    '[1, 2]',
    '... @l.grep(-> $x { $x <= 2 }) == [1, 2]');  

pil_is_eq($prelude ~
    '@l.reduce(-> $x, $y { $x`add($y) })',
    '6',
    '... @l.reduce(-> $x, $y { $x`add($y) }) == 6');     

pil_is_eq($prelude ~
    '@l.zip(^List.new([ "a", "b", "c" ])).FETCH_LIST()',
    '[1, "a", 2, "b", 3, "c"]',
    '... @l.zip(^List.new([ "a", "b", "c" ])) == [1, "a", 2, "b", 3, "c"]');     

pil_is_eq(
    '^List.new([5, 9, 20, 222, 102, 3, 44, 8, 17, 10101]).sort().FETCH_LIST()',
    '[3, 5, 8, 9, 17, 20, 44, 102, 222, 10101]',
    '... List sort works now too');
