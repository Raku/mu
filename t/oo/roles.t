#!/usr/bin/pugs

use v6;
require Test;

plan 15;

=pod

Basic role tests from L<S12/"Roles">

=cut

# L<S12/"Roles">
# Basic definition
todo_eval_ok 'role Foo {}',           "definition of a role worked";
todo_eval_ok 'class Bar does Foo {}', "definition of a class which does a role worked";

# Smartmatch and .meta.does
my $bar = eval 'Bar.new()';
todo_eval_ok '$bar ~~ Bar',         '... smartmatch our $bar to the Bar class';
todo_eval_ok '$bar.meta.does(Foo)', '.meta.does said our $bar does Foo';
todo_eval_ok '$bar ~~ Foo',         'smartmatch said our $bar does Foo';

# Mixing a Role into an Object using imperative C<does>
my $baz = 3;
todo_eval_ok '$baz does Foo',       'mixing in our Foo role into $baz worked';
todo_eval_ok '$baz.meta.does(Foo)', '.meta.does said our $baz now does Foo';
todo_eval_ok '$baz ~~ Baz',         'smartmatch said our $baz now does Foo';

# L<S12/"Roles" /"but with a C<role> keyword:"/>
# Roles may have methods
todo_eval_ok 'role A { method say_hello(Str $to) { "Hello, $to" } }',
  "definition of a role with a method worked";
todo_eval_ok 'my Foo $a does A', 'mixing A into $a worked';
todo_eval_is '$a.say_hello("Ingo")', "Ingo",
  'our $a "inherited" the .say_hello method of A';

# L<S12/"Roles" /"Roles may have attributes:"/>
todo_eval_ok 'role B { has $.attr = 42 is rw }',
  "definition of a role with an attribute worked";
todo_eval_ok 'my Foo $b does B', 'mixing B into $b worked';
todo_eval_is '$b.attr', 42,      'our $b "inherited" the $.attr attribute of B (1)';
todo_eval_is '$b.attr = 23', 23, 'our $b "inherited" the $.attr attribute of B (2)';
