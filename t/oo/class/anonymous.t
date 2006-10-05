use v6-alpha;

use Test;

plan 5;

=pod

Anonymous class tests.

=cut

my $class;
ok eval('$class = class { method meth() { return 42 } }'),
  "anonymous class creation", :todo<feature>;
ok eval('$class ~~ Class'), "an anonymous class isa Class", :todo<feature>;

my $a;
ok eval('$a = $class.new'), "instantiation of anonymous class", :todo<feature>;
is eval('$a.meth'), 42,
  "calling a method on an instance of an anonymous class (1)", :todo<feature>;

# And the same w/o using a $class variable:
is eval('class { method meth() { return 42 } }.new.meth'), 42,
  "calling a method on an instance of an anonymous class (2)", :todo<feature>;
