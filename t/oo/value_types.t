#!/usr/bin/pugs

use v6;
use Test;

# Reference:
# Thread "How do I... create a value type" started by Ingo Blechschmidt
# (http://www.nntp.perl.org/group/perl.perl6.language/22217)

plan 5;

class MyValType is value {
  has $.data;

  method succ () { $.data + 1 }
}

{
  my MyValType $foo .= new(:data<42>);
  is $foo.data, 42,     "instantiating an own value type worked (1)";
  ok $foo ~~ MyValType, "instantiating an own value type worked (2)";

  my MyValType $bar = $foo;
  ok !($bar =:= $foo),  "assigning a value type gets you a copy (1)", :todo<feature>;
  $bar.data = 23;
  is $bar.data, 23,     "modifying a value type object worked";
  is $foo.data, 42,     "assigning a value type gets you a copy (2)", :todo<feature>;
}
