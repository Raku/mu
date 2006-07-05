use v6-pugs;

use Test;

plan 1;

class A is B { method f {1} };
class B { method g { ./f } };

is(A.g(), 1, 'inheritance works on class methods');

