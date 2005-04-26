#!/usr/bin/pugs

use v6;
use Test;

plan 3;


# L<S06/"The &?SUB routine" /contains the name of the current subroutine/>
# L<S02/"Names" /Which sub name am I in/>
sub foo { return $?SUBNAME } 
is(foo(), '&foo', 'got the right subname');

my $bar = sub { return $?SUBNAME };
is($bar(), '<anon>', 'got the right subname (anon-block)');

my $baz = eval '$?SUBNAME';
ok(not defined $baz, '$?SUBNAME not defined outside of a sub');
