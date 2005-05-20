#!/usr/bin/pugs

use v6;
use Test;

plan 8;


# L<S02/Out-of-scope names>
dies_ok( { module MY }, "MY is an out of scope name", :todo<bug> );
dies_ok( { module OUTER }, "OUTER is an out of scope name", :todo<bug> );
dies_ok( { module CALLER }, "CALLER is an out of scope name", :todo<bug>  );


# L<S04/The Relationship of Blocks and Declarations>
dies_ok({ my $x; my $x }, 'its illegal to declare $x twice in the same scope.', :todo<bug> );
dies_ok({ state $x; state $x }, 'its illegal to declare $x twice in the same scope.', :todo<bug> );
{ my $a = 1; {
   my $a=2; {
      my $a=3;
      ok $a==3,                  'get regular a'; 
      ok $OUTER::a ==2,          'get $OUTER::a'; 
      ok  $OUTER::OUTER::a == 1, 'get $OUTER::OUTER::a';
}}}
