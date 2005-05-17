#!/usr/bin/pugs

use v6;
use Test;

plan 5;

# L<S06/"Operator overloading">
eval_ok '
class Foo {
    has $.bar;
    method prefix:<~> ($self) { return $.bar }
    method infix:<+> ($a, $b) { return "$a $b" }
}; 1
', 'class definition with operator overloading method worked';

{
    my $val;
    lives_ok {
        my $foo = Foo.new();
        $foo.bar = 'hate';
        $val = "$foo.bar"
    }, '... class methods work for class', :todo<feature>;
    is($val, 'software', '... basic operator overloading worked', :todo<feature>);

    lives_ok {
        my $foo = Foo.new();
        $foo.bar = 'hate';
        $val = $foo + $foo;
    }, '... class methods work for class', :todo<feature>;
    is($val, 'software software', '... basic operator overloading worked', :todo<feature>);
}
