#!/usr/bin/pugs

use v6;

=pod

Modification of list attributes created with constructor fails

=cut

use Test;
plan 6;

class Foo { 
   has @.test is rw; 
   method get () { shift @.test }
}

my $test1 = Foo.new();
$test1.test = [1];
is($test1.test, [1], "Initialized outside constructor");
is($test1.get ,  1 , "Get appears to have worked");
is($test1.test,  [], "Get Worked!");

my $test2 = Foo.new( :test([1]) );
is($test2.test, [1], "Initialized inside constructor");
is($test2.get ,  1 , "Get appears to have worked");
is($test2.test,  [], "Get Worked!", :todo<bug>);

