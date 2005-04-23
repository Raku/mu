#!/usr/bin/pugs

use v6;
require Test;

plan 4;

{
  eval_ok '
    sub foo($a is rw) {
      $a = 42;
      return 19;
    }
  ', "definition of a subroutine with an is rw param";

  my $bar = 23;
  is           $bar,        23, "basic sanity";
  todo_eval_is 'foo($bar)', 19, "calling a sub with an is rw param";
  todo_is      $bar,        42, "sub changed our variable";
}

# for ... -> ... is rw {...} already tested for in t/statements/for.t.
