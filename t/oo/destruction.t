#!/usr/bin/pugs

use v6;
require Test;

plan 5;

# L<A12/"Object Deconstruction">

my $in_destructor = 0;

eval_ok 'class Foo { submethod DESTROY { $in_destructor++ } }',
  "class definition", :todo(1);

my $a;
eval_ok '$a = Foo.new()',  "basic instantiation", :todo(1);
eval_ok '$a ~~ Foo',       "smartmatching the class name", :todo(1);
# As usual, is instead of todo_is to suppress unexpected succeedings.
is           $in_destructor, 0, "own destructor was not yet called";
undef $a;
is      $in_destructor, 1, "own destructor was called", :todo(1);
