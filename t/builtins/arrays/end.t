#!/usr/bin/pugs

use v6;
use Test;

=kwid

Array .end tests

=cut

plan 6;

# basic array .end tests

{
    my @array = ();
    is(@array.end, -1, 'we have an empty list');

    @array = (1..43);
    is(@array.end, 42, 'index of last element is 42 after assignment');

    pop @array;
    is(@array.end, 41, 'index of last element is 41 after pop');

    shift @array;
    is(@array.end, 40, 'index of last element is 40 after shift');

    unshift @array, 'foo';
    is(@array.end, 41, 'index of last element is 41 after unshift');

    push @array, 'bar';
    is(@array.end, 42, 'index of last element is 42 after push');
}
