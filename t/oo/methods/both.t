#!/usr/bin/pugs

use v6;
require Test;

plan 3;

# L<A12/"Class|object Invocant">
todo_eval_ok '
  class Foo {
    method blarb(Class|Foo $class: $arg) {
      return 100 + $arg;
    }
  }
', "basic class and class method definition worked";
todo_eval_is 'A.blarb(42)', 142, "basic class method access worked";
# Double eval() needed to bypass smart compilers :)
todo_eval_is 'A.new.blarb(42)', 142,
  "class|instance methods work on instances, too";
