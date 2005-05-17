#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# L<S06/"Operator overloading">
eval_ok '
class Foo {
    has $.bar;
    method as_string ($self:) { return $.bar }
};
', 'class definition with operator overloading method worked';

{
    my $val;
    lives_ok {
        my $foo = Foo.new();
        $foo.bar = 'software';
        $val = "$foo.bar"
    }, '... class methods work for class', :todo<feature>;
    is($val, 'software', '... basic operator overloading worked', :todo<feature>);
}
