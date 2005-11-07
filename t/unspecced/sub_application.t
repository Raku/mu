#!/usr/bin/pugs

use v6;
use Test;

plan 4;

=pod

    $foo(42);  # is sugar for
    $foo.postcircumfix:<( )>(42);

=cut

# Example: Make $foo() a noop.
{
    my $foo = 42;
    dies_ok { $foo() }, "basic sanity";

    eval '$foo does role {
        method postcircumfix:<( )> {}
    }';
    lives_ok { $foo() }, "overriding postcircumfix:<( )> (1)", :todo<feature>;
}

# Example: Make $foo() modify another variable.
{
    my $foo = 42;
    my $bar = 23;

    eval '$foo does role {
        method postcircumfix:<( )> {
            $bar++;
        }
    }';
    try { $foo() };
    is $bar, 24, "overriding postcircumfix:<( )> (2)", :todo<feature>;
}

# .postcircumfix:<( )> is called even when you don't actually use ()s to
# call, as
#   foo;    # is merely sugar for
#   foo();
{
    my $bar;
    my sub foo {
        # This body should never be called!
        $bar = 23
    };
    eval '&foo does role {
        method postcircumfix:<( )> {
            $bar = 42;
        }
    }';

    foo;
    is $bar, 42,
        ".postcircumfix:<( )> is called even when you don't actually use ()s to call",
        :todo<feature>;
}
