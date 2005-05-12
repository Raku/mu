#!/usr/bin/pugs

use v6;
use Test;

=kwid

Splatted parameters shouldn't be rw even if stated as such

=cut

plan 3;

# test splatted parameter for rw ability

my @test = 1..5;
eval '
    sub should_fail ( *@list is rw ) {
        @list[0] = "failure expected"; 
    }
	should_fail(@test);
';

ok(defined($!), "trying to use an 'is rw' splat doesn't work out");
is(@test[0], 1, "@test was unchanged");

eval '
	sub should_fail (*@list is rw) { }
';

ok(defined($!), "trying to define an 'is rw' splat doesn't work either", :todo<feature>);



