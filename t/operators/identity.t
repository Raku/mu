#!/usr/bin/pugs

use v6;
use Test;

plan 4;

{
  my $a = 3;
  my $b = 3;

  ok $a == $b,              "our two variables are equal";
  ok eval 'not($a =:= $b)', "our two variables are not identity equal", :todo<feature>;
}

{
  my $sub = -> $arg1, $arg2 { eval '$arg1 =:= $arg2' };
  my $a   = 3;
  my $b   = 3;

  ok !$sub($a, $b), "our two variables are not identity equal when passed to a sub";
  ok $sub($a, $a),  "our two variables are identity equal when passed to a sub", :todo<feature>;
}
