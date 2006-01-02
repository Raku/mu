#!./pugs

use v6;
use Test::PIL::Bootstrap;

pil_is_eq('::Pair.has_method("key")', 'true', '... ::Pair.has_method(key)');
pil_is_eq('::Pair.has_method("value")', 'true', '... ::Pair.has_method(value)');
pil_is_eq('::Pair.has_method("kv")', 'true', '... ::Pair.has_method(kv)');

pil_is_eq(
    '::Pair.new({}).key', 
    'nil', 
    '... Pair.new().key == nil');

pil_is_eq(
    '::Pair.new({}).value', 
    'nil', 
    '... Pair.new().value == nil');    

pil_is_eq(
    '::Pair.new({ "$!key" => "key", "$!value" => "value" }).key', 
    '"key"', 
    '... Pair.new(key => value).key == key');
    
pil_is_eq(
    '::Pair.new({ "$!key" => "key", "$!value" => "value" }).value', 
    '"value"', 
    '... Pair.new(key => value).value == value');  
    
pil_is_eq(
    '$p := ::Pair.new({ "$!key" => "key", "$!value" => "value" }); $p.kv`eq($p)', 
    'true', 
    '... $p := Pair.new(key => value); $p.kv == $p');        