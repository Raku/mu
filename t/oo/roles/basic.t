#!/usr/bin/pugs

use v6;
require Test;

plan 20;

=pod

Basic role tests from L<S12/"Roles">

=cut

# L<S12/"Roles">
# Basic definition
eval_ok 'role Foo {}',           "definition of a role worked", :todo(1);
eval_ok 'class Bar does Foo {}', "definition of a class which does a role worked", :todo(1);

# Smartmatch and .meta.does
my $bar = eval 'Bar.new()';
eval_ok '$bar ~~ Bar',         '... smartmatch our $bar to the Bar class', :todo(1);
eval_ok '$bar.meta.does(Foo)', '.meta.does said our $bar does Foo', :todo(1);
eval_ok '$bar ~~ Foo',         'smartmatch said our $bar does Foo', :todo(1);

# Mixing a Role into an Object using imperative C<does>
my $baz = 3;
eval_ok '$baz does Foo',       'mixing in our Foo role into $baz worked', :todo(1);
eval_ok '$baz.meta.does(Foo)', '.meta.does said our $baz now does Foo', :todo(1);
eval_ok '$baz ~~ Baz',         'smartmatch said our $baz now does Foo', :todo(1);

# L<S12/"Roles" /but with a role keyword:/>
# Roles may have methods
eval_ok 'role A { method say_hello(Str $to) { "Hello, $to" } }',
  "definition of a role with a method worked", :todo(1);
eval_ok 'my Foo $a does A .= new()', 'mixing A into $a worked', :todo(1);
eval_is '$a.say_hello("Ingo")', "Ingo",
  'our $a "inherited" the .say_hello method of A', :todo(1);

# L<S12/"Roles" /Roles may have attributes:/>
eval_ok 'role B { has $.attr = 42 is rw }',
  "definition of a role with an attribute worked", :todo(1);
eval_ok 'my Foo $b does B .= new()', 'mixing B into $b worked', :todo(1);
eval_is '$b.attr', 42,      'our $b "inherited" the $.attr attribute of B (1)', :todo(1);
eval_is '$b.attr = 23', 23, 'our $b "inherited" the $.attr attribute of B (2)', :todo(1);

# L<S12/"Roles" /operator creates a copy and works on that./>
# As usual, ok instead of todo_ok to avoid unexpected succeedings.
eval_ok 'my Foo $c .= new()',        'creating a Foo worked', :todo(1);
ok           !eval('$c ~~ B'),            '$c does not B';
eval_ok 'my $d = $c but B',          'mixing in a Role via but worked', :todo(1);
ok           !eval('$c ~~ B'),            '$c still does not B...';
eval_ok '$d ~~ B',                   '...but $d does B', :todo(1);
