#!/usr/bin/pugs

use v6;
require Test;

plan 7;

# L<S12/"Construction and Initialization">

my $in_own = 0;
todo_eval_ok '
  class OwnConstructor {
    has $.x = 13;
    method own(Class $class) {
      $in_own++;
      return $class.bless(:x(42));
    }
  }
', "class definition worked";
todo_eval_ok 'OwnConstr.new ~~ OwnConstr', "basic class instantiation";
todo_eval_is 'OwnConstr.new.x', 13,        "basic attribute access";
# As usual, is instead of todo_is to suppress unexpected succeedings
is           $in_own, 0,                   "own constructor was not called";
todo_eval_ok 'OwnConstr.own ~~ OwnConstr', "own construction instantiated its class";
todo_eval_is 'OwnConstr.own.x', 42,        "attribute was set from our constructor";
todo_is      $in_own, 1,                   "own constructor was actually called";
