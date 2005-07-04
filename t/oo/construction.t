#!/usr/bin/pugs

use v6;
use Test;

plan 9;

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
', "class definition worked", :todo<feature>;
eval_ok 'OwnConstr.new ~~ OwnConstr', "basic class instantiation", :todo<feature>;
eval_is 'OwnConstr.new.x', 13,        "basic attribute access", :todo<feature>;
# As usual, is instead of todo_is to suppress unexpected succeedings
is      $in_own, 0,                   "own constructor was not called";
eval_ok 'OwnConstr.own ~~ OwnConstr', "own construction instantiated its class", :todo<feature>;
eval_is 'OwnConstr.own.x', 42,        "attribute was set from our constructor", :todo<feature>;
is      $in_own, 1,                   "own constructor was actually called", :todo<feature>;


# L<http://www.mail-archive.com/perl6-language@perl.org/msg20241.html>
# provide constructor for single positional argument

class Foo {
  has $.a;
  
  method new (Class $self: Str $string) {
    $.a = $string;
    return $self.bless(string => $string);
  }
}


ok 'Foo.new("a string") ~~ Foo', '... our Foo instance was created';

eval_is 'Foo.new("a string").a', 'a string', "our own 'new' was called", :todo<feature>;

