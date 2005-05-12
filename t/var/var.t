#!/usr/bin/pugs

use v6;
use Test;

plan 6;


# L<S02/Out-of-scope names>
dies_ok( { module MY }, "MY is an out of scope name", :todo<bug> );
dies_ok( { module OUTER }, "OUTER is an out of scope name", :toto<bug> );
dies_ok( { module CALLER }, "CALLER is an out of scope name", :todo<bug>  );


# L S<o4/The Relationship of Blocks and Declarations>
dies_ok( { my $x; my $x }, "it's illegal to declare $x twice in the same scope.", :todo<bug> );
dies_ok( { local $x; local $x }, "it's illegal to declare $x twice in the same scope.", :todo<bug> );
dies_ok( { state $x; state $x }, "it's illegal to declare $x twice in the same scope.", :todo<bug> );
