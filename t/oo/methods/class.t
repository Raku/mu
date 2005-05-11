#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# L<A12/"Class Methods" /such as a constructor, you say something like:/>

class Foo {
    method bar (Class $class: $arg) { return 100 + $arg }
}

{
    my $val;
    lives_ok {
        $val = Foo.bar(42);
    }, '... class methods work for class', :todo<feature>;
    is($val, 142, '... basic class method access worked', :todo<feature>);
}

{
    my $foo = Foo.new();
    my $val;
    # NOTE:
    # this dies for the wrong reason actually
    dies_ok {
        $val = $foo.bar(42);
    }, '... class methods should not work for instances';
}
