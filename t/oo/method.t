#!/usr/bin/pugs

use v6;
require Test;

plan 7;

=pod

Very basic class method tests from L<S12/"Methods">

=cut

# L<S12/"Methods" /"either the dot notation or indirect object notation:">
eval 'class Foo {
  method doit ($a, $b, $c) { $a + $b + $c }
  method noargs () { 42 }
}';

my $foo = eval 'Foo.new()';
todo_eval_is '$foo.doit(1,2,3)', 6, "dot method invocation";
todo_eval_is 'doit $foo: 1,2,3', 6, "indirect method invocation";

todo_eval_is '$foo.noargs',   42,     "parentheses after method (1)";
todo_eval_is '$foo.noargs()', 42,     "parentheses after method (2)";
# ok instead of todo_ok to suppress "unexpected succeeded"-messages
ok           !eval('$foo.noargs ()'), "parentheses after method (3)";
todo_eval_is '$foo.noargs.()', 42,    "parentheses after method (4)";
todo_eval_is '$foo.noargs .()', 42,   "parentheses after method (5)";
