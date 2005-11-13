#!/usr/bin/pugs

use v6;
use Test;

=pod

We ought to be able to change a value when aliasing into it.

=cut

plan 8;

{
    my %h = 1..4;
    lives_ok {
        for %h.values -> $v is rw { $v += 1 }
    }, 'aliases returned by %hash.values should be rw (1)', :todo<bug>;

    is %h<3>, 5, 'aliases returned by %hash.values should be rw (2)', :todo<bug>;
}

{
    my @a = 1..4;
    lives_ok {
        for @a.values -> $v is rw { $v += 1 }
    }, 'aliases returned by @array.values should be rw (1)', :todo<bug>;

    is @a[2], 4, 'aliases returned by @array.values should be rw (2)', :todo<bug>;
}

{
    my $pair = (a => 42);
    lives_ok {
        for $pair.values -> $v is rw { $v += 1 }
    }, 'aliases returned by $pair.values should be rw (1)', :todo<bug>;

    is $pair.values, 43, 'aliases returned by $pair.values should be rw (2)', :todo<bug>;
}

{
    my $var  = 42;
    my $pair = (a => $var);
    lives_ok {
        for $pair.values -> $v is rw { $v += 1 }
    }, 'aliases returned by $pair.values should be rw (1)', :todo<bug>;

    is $pair.values, 43, 'aliases returned by $pair.values should be rw (2)', :todo<bug>;
}

# (currently this dies with "Can't modify constant item: VInt 2")
