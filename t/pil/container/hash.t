#!./pugs

use v6;
use Test::PIL::Bootstrap;

pil_is_eq('::Hash.has_method("FETCH")', 'true', '... ::Hash.has_method(FETCH)');
pil_is_eq('::Hash.has_method("STORE")', 'true', '... ::Hash.has_method(STORE)');

pil_is_eq('::Hash.does("Hash")', 'true', '... ::Hash.does(Array)');