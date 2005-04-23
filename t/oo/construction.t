#!/usr/bin/pugs

use v6;
require Test;

plan 7;

# L<S12/"Construction and Initialization">

my $in_own = 0;
eval_ok '
  class OwnConstructor {
    has $.x = 13;
    method own(Class $class) {
      $in_own++;
      return $class.bless(:x(42));
    }
  }
', "class definition worked", :todo(1);
eval_ok 'OwnConstr.new ~~ OwnConstr', "basic class instantiation", :todo(1);
eval_is 'OwnConstr.new.x', 13,        "basic attribute access", :todo(1);
# As usual, is instead of todo_is to suppress unexpected succeedings
is           $in_own, 0,                   "own constructor was not called";
eval_ok 'OwnConstr.own ~~ OwnConstr', "own construction instantiated its class", :todo(1);
eval_is 'OwnConstr.own.x', 42,        "attribute was set from our constructor", :todo(1);
is      $in_own, 1,                   "own constructor was actually called", :todo(1);
