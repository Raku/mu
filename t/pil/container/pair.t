#!./pugs

use v6;
use Test::PIL::Bootstrap;

pil_is_eq('^Pair.identifier()', '"Pair-0.0.1-url:pugscode.org"', '... Pair has the correct identifier');

pil_is_eq('^Pair.has_method("key")', 'true', '... ^Pair.has_method(key)');
pil_is_eq('^Pair.has_method("value")', 'true', '... ^Pair.has_method(value)');
pil_is_eq('^Pair.has_method("kv")', 'true', '... ^Pair.has_method(kv)');

pil_is_eq(
    '^Pair.new({}).key', 
    'nil', 
    '... Pair.new().key == nil');

pil_is_eq(
    '^Pair.new({}).value', 
    'nil', 
    '... Pair.new().value == nil');    

pil_is_eq(
    '^Pair.new({ "$!key" => "key", "$!value" => "value" }).key', 
    '"key"', 
    '... Pair.new(key => value).key == key');
    
pil_is_eq(
    '^Pair.new({ "$!key" => "key", "$!value" => "value" }).value', 
    '"value"', 
    '... Pair.new(key => value).value == value');  
    
pil_is_eq(
    '^Pair.new({ "$!key" => "key", "$!value" => "value" }).kv`fetch_list()', 
    '["key", "value"]', 
    '... Pair.new(key => value).kv == [key, value]');        
    
# use the k and v constructor shortcuts    
    
pil_is_eq(
    '^Pair.new({ "k" => "key", "v" => "value" }).key', 
    '"key"', 
    '... Pair.new(key => value).key == key');

pil_is_eq(
    '^Pair.new({ "k" => "key", "v" => "value" }).value', 
    '"value"', 
    '... Pair.new(key => value).value == value');  

pil_is_eq(
    '^Pair.new({ "k" => "key", "v" => "value" }).kv`fetch_list()', 
    '["key", "value"]', 
    '... Pair.new(key => value).kv == [key, value]');            