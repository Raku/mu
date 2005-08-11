#!/usr/bin/pugs

use v6;
use Test;

plan 1;

class Foo {}

my $num_objects = 20;

my %foos;
for (1 .. $num_objects) {
    my $f = Foo.new();
    %foos{$f.id()}++;
}

is(+%foos, $num_objects, '... all our .id()s were unique');
