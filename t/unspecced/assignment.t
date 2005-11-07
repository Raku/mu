#!/usr/bin/pugs

use v6;
use Test;

plan 6;

=pod

    $foo = 42;  # is sugar for
    $foo.infix:<=>(42);

=cut

{
    my $foo = 42;
    try { $foo.infix:<=>(23) };
    is $foo, 23, "basic scalar assignment using .infix:<=>";
}

{
    my $foo = 42;
    try { my @array = <a b c>; $foo.infix:<=>(@array) };
    is ~$foo, "a b c", "scalar assignment using .infix:<=>";
}

=pod

    ($foo, $bar) = (23, 42);     # is sugar for
    ($foo, $bar).infix:<=>(42);  # which in turn is sugar for
    infix:<,>($foo, $bar).infix:<=>(42);
    # &infix:<,> returns an appropriate rw proxy object.

=cut

{
    my ($foo, $bar);
    try { ($foo, $bar).infix:<=>(13, 14) };

    is $foo, 13, "array assignment using .infix:<=> (1)", :todo<feature>;
    is $bar, 14, "array assignment using .infix:<=> (2)", :todo<feature>;
}

# Overriding infix:<=>
{
    my $foo = 42;
    my $bar = 23;
    eval '$foo does role {
        method infix:<=> {
            $bar++;
        }
    }';

    $foo = "new";
    is $foo, 42, "overriding infix:<=> (1)", :todo<feature>;
    is $bar, 24, "overriding infix:<=> (2)", :todo<feature>;
}
