#!/usr/bin/pugs

use v6;
use Test;

plan 8;


# L<S02/Out-of-scope names>
dies_ok( { module MY }, "MY is an out of scope name", :todo<bug> );
dies_ok( { module OUTER }, "OUTER is an out of scope name", :todo<bug> );
dies_ok( { module CALLER }, "CALLER is an out of scope name", :todo<bug>  );


# L<S04/The Relationship of Blocks and Declarations>
dies_ok({ my $x; my $x }, 'it is illegal to declare $x twice in the same scope.', :todo<bug> );
dies_ok({ state $x; state $x }, 'it is illegal to declare $x twice in the same scope.', :todo<bug> );
{ my $a = 1; {
   my $a=2; {
      my $a=3;
      Test::is($a, 3,               'get regular a'); 
      Test::is($OUTER::a, 2,        'get $OUTER::a'); 
      Test::is($OUTER::OUTER::a, 1, 'get $OUTER::OUTER::a', :todo<bug>);
}}}
