#!/usr/bin/pugs

use v6;
use Test;

plan 3;

# L<A12/"Class Methods" /such as a constructor, you say something like:/>
eval_ok '
  class Foo {
    method blarb(Class $class: $arg) {
      return 100 + $arg;
    }
  }
', "basic class and class method definition worked", :todo<feature>;
eval_is 'Foo.blarb(42)', 142, "basic class method access worked", :todo<feature>;
# Double eval() needed to bypass smart compilers :)
eval_ok '!try { eval "Foo.new.blarb(42)" }',
  "pure class methods don't work on instances", :todo<feature>;
