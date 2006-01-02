#!./pugs

use v6;
use Test::PIL::Bootstrap;

pil_is_eq(
    '::Array.identifier()', 
    '"Array-0.0.1-url:pugscode.org"', 
    '... Array has the correct identifier');

pil_is_eq('::Array.has_method("FETCH")', 'true', '... ::Array.has_method(FETCH)');
pil_is_eq('::Array.has_method("STORE")', 'true', '... ::Array.has_method(STORE)');

pil_is_eq('::Array.does("Array")', 'true', '... ::Array.does(Array)');

for (qw(
    elems join map grep pop push shift unshift reverse sort kv
    )) -> $method_name {
    pil_is_eq(
        '::Array.has_method("' ~ $method_name ~ '")', 
        'true', 
        '... ::Array.has_method(' ~ $method_name ~ ')');
}

my $prelude = q:to/PRELUDE/
@a := ::Array`create([ 1, 2, 3 ]);
PRELUDE;

pil_is_eq($prelude ~
    '@a.elems()',
    '3',
    '... @a.elems == 3');
    
pil_is_eq($prelude ~
    '@a.join(", ")',
    '"1, 2, 3"',
    '... @a.join(", ") == 1, 2, 3');    
    
pil_is_eq($prelude ~
    '[ @a.pop(), @a.pop(), @a.pop() ]',
    '[3, 2, 1]',
    '... @a.pop(), @a.pop(), @a.pop() == [3, 2, 1]');  
    
pil_is_eq($prelude ~
    '@a.push(4)`fetch_list()',
    '[1, 2, 3, 4]',
    '... @a.push(4) == [1, 2, 3, 4]');            
    
pil_is_eq($prelude ~
    '[ @a.shift(), @a.shift(), @a.shift() ]',
    '[1, 2, 3]',
    '... @a.shift(), @a.shift(), @a.shift() == [1, 2, 3]');            
    
pil_is_eq($prelude ~
    '@a.unshift(0)`fetch_list()',
    '[0, 1, 2, 3]',
    '... @a.unshift(0) == [0, 1, 2, 3]');    
    
pil_is_eq($prelude ~
    '@a.reverse()`fetch_list()',
    '[3, 2, 1]',
    '... @a.reverse() == [3, 2, 1]');  
    
pil_is_eq($prelude ~
    '@a.map(-> $x { $x`add(2) })`fetch_list()',
    '[3, 4, 5]',
    '... @a.map(-> $x { $x + 2 }) == [3, 4, 5]');      
    
pil_is_eq($prelude ~
    '@a.grep(-> $x { $x`le(2) })`fetch_list()',
    '[1, 2]',
    '... @a.grep(-> $x { $x <= 2 }) == [1, 2]');          