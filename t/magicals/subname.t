#!/usr/bin/pugs

use v6;
require Test;

plan 2;

sub foo { return $?SUBNAME } 
is(foo(), '&foo', 'got the right subname');

my $bar = sub { return $?SUBNAME };
is($bar(), '<anon>', 'got the right subname (anon-block)');