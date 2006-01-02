#!./pugs

use v6;
use Test::PIL::Bootstrap;

pil_is_eq('::Scalar.has_method("FETCH")', 'true', '... ::Scalar.has_method(FETCH)');
pil_is_eq('::Scalar.has_method("STORE")', 'true', '... ::Scalar.has_method(STORE)');

pil_is_eq('::Scalar.does("Scalar")', 'true', '... ::Scalar.does(Scalar)');