#!/usr/bin/pugs

use v6;
require Test;

plan 2;


# L<S06/"The &?SUB routine" /SUBNAME contains the name of the current subroutine/>
# L<S02/"Names" /Which sub name am I in/>
sub foo { return $?SUBNAME } 
is(foo(), '&foo', 'got the right subname');

my $bar = sub { return $?SUBNAME };
is($bar(), '<anon>', 'got the right subname (anon-block)');
