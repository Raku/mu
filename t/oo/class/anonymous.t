#!/usr/bin/pugs

use v6;
require Test;

plan 5;

=pod

Anonymous class tests.

=cut

my $class;
todo_eval_ok '$class = class { method meth() { return 42 } }',
  "anonymous class creation";
todo_eval_ok '$class ~~ Class', "an anonymous class isa Class";

my $a;
todo_eval_ok '$a = $class.new', "instantiation of anonymous class";
todo_eval_is '$a.meth', 42,
  "calling a method on an instance of an anonymous class (1)";

# And the same w/o using a $class variable:
todo_eval_is 'class { method meth() { return 42 } }.new.meth', 42,
  "calling a method on an instance of an anonymous class (2)";
