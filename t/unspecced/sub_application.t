#!/usr/bin/pugs

use v6;
use Test;

plan 3;

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
    lives_ok { $foo() }, "overriding postcircumfix:<( )> (1)";
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
    ok $bar, "overriding postcircumfix:<( )> (2)";
}
