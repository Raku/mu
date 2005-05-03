#!/usr/bin/pugs

use v6;
use Test;

plan 5;

=pod

Anonymous class tests.

=cut

my $class;
eval_ok '$class = class { method meth() { return 42 } }',
  "anonymous class creation", :todo<feature>;
eval_ok '$class ~~ Class', "an anonymous class isa Class", :todo<feature>;

my $a;
eval_ok '$a = $class.new', "instantiation of anonymous class", :todo<feature>;
eval_is '$a.meth', 42,
  "calling a method on an instance of an anonymous class (1)", :todo<feature>;

# And the same w/o using a $class variable:
eval_is 'class { method meth() { return 42 } }.new.meth', 42,
  "calling a method on an instance of an anonymous class (2)", :todo<feature>;
