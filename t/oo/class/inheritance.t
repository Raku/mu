use v6-alpha;

use Test;

plan 1;

class A is B { method f {1} };
class B { method g { self.f } };

is(A.g(), 1, 'inheritance works on class methods');

