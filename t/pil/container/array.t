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
    elems join map grep reduce zip pop push shift unshift reverse sort keys values kv
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
    
pil_is_eq($prelude ~
    '@a.reduce(-> $x, $y { $x`add($y) })',
    '6',
    '... @a.reduce(-> $x, $y { $x`add($y) }) == 6');     
    
pil_is_eq($prelude ~
    '@a.zip(::Array`create([ "a", "b", "c" ]))`fetch_list()',
    '[1, "a", 2, "b", 3, "c"]',
    '... @a.zip(::Array`create([ "a", "b", "c" ])) == [1, "a", 2, "b", 3, "c"]');     
 
pil_is_eq($prelude ~
    '@a.keys()`fetch_list()',
    '[0, 1, 2]',
    '... @a.keys() == [0, 1, 2]'); 
    
pil_is_eq($prelude ~
    '@a.values()`fetch_list()',
    '[1, 2, 3]',
    '... @a.keys() == [1, 2, 3]'); 
    
pil_is_eq($prelude ~
    '@a.kv().map(-> $x { $x`fetch_list() })`fetch_list()',
    '[[0, 1], [1, 2], [2, 3]]',
    '... @a.kv() == [[0, 1], [1, 2], [2, 3]]');    
    
pil_is_eq($prelude ~
    '@a.pairs().map(-> $x { [ $x.key(), $x.value() ] })`fetch_list()',
    '[[0, 1], [1, 2], [2, 3]]',
    '... @a.pairs() == [(0 => 1), (1 => 2), (2 => 3)]');          
          
pil_is_eq(
    '::Array`create([5, 9, 20, 222, 102, 3, 44, 8, 17, 10101]).sort()`fetch_list()',
    '[3, 5, 8, 9, 17, 20, 44, 102, 222, 10101]',
    '... array sort works now too');