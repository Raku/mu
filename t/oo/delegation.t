#!/usr/bin/pugs

use v6;
require Test;

plan 34;

=pod

Delegation tests from L<S12/"Delegation">

=cut

# L<S12/"Delegation">

todo_eval_ok '
  class Backend1 { method hi() { 42 } method cool() { 1337 } }
  class Backend2 { method hi() { 23 } method cool() {  539 } }
  class Frontend { has $.backend is rw handles "hi" }
', "class definition worked";

todo_eval_is 'Backend1.new.hi', 42, "basic sanity (1)";
todo_eval_is 'Backend2.new.hi', 23, "basic sanity (2)";

{
  my $a;
  todo_eval_ok '$a = Frontend.new', "basic instantiation worked (1)";
  todo_eval_ok '!try { $a.hi }', "calling a method on no object didn't succeed (1)";
  todo_eval_ok '$a.backend = Backend1.new()', "setting a handler object (1)";
  todo_eval_ok '!$a ~~ Backend1',             "object wasn't isa()ed (1)";
  todo_eval_is '$a.hi', 42, "method was successfully handled by backend object (1)";
}

{
  my $a;
  todo_eval_ok '$a = Frontend.new', "basic instantiation worked (2)";
  todo_eval_ok '!try { $a.hi }', "calling a method on no object didn't succeed (2)";
  todo_eval_ok '$a.backend = Backend2.new()', "setting a handler object (2)";
  todo_eval_ok '!$a ~~ Backend2',             "object wasn't isa()ed (2)";
  todo_eval_is '$a.hi', 23, "method was successfully handled by backend object (2)";
}


# L<S12/"Delegation" /Any other kind of argument to handles is considered to be a smartmatch selector for methods/>
todo_eval_ok 'class ReFrontend { has $.backend is rw handles /^hi/ }',
  "class definition using a smartmatch handle worked";
{
  my $a;
  todo_eval_ok '$a = ReFrontend.new', "basic instantiation worked (3)";
  todo_eval_ok '!try { $a.hi }', "calling a method on no object didn't succeed (3)";
  todo_eval_ok '$a.backend = Backend1.new()', "setting a handler object (3)";
  todo_eval_ok '!$a ~~ Backend1',             "object wasn't isa()ed (3)";
  todo_eval_is '$a.hi', 42, "method was successfully handled by backend object (3)";
}


# L<S12/"Delegation" /If you say/>
todo_eval_ok 'class ClassFrontend { has $.backend is rw handles Backend2 }',
  "class definition using a Class handle worked";
{
  my $a;
  todo_eval_ok '$a = ClassFrontend.new', "basic instantiation worked (4)";
  todo_eval_ok '!try { $a.hi }', "calling a method on no object didn't succeed (4)";
  todo_eval_ok '$a.backend = Backend1.new()', "setting a handler object (4)";
  todo_eval_ok '!$a ~~ Backend1',             "object wasn't isa()ed (4-1)";
  todo_eval_ok '!$a ~~ Backend2',             "object wasn't isa()ed (4-2)";
  todo_eval_is '$a.hi', 42, "method was successfully handled by backend object (4)";
}


# L<S12/"Delegation" /You can specify multiple method names:>
todo_eval_ok 'class MultiFrontend { has $.backend is rw handles <hi cool> }',
  "class definition using multiple method names worked";
{
  my $a;
  todo_eval_ok '$a = MultiFrontend.new', "basic instantiation worked (5)";
  todo_eval_ok '!try { $a.hi   }', "calling a method on no object didn't succeed (5-1)";
  todo_eval_ok '!try { $a.cool }', "calling a method on no object didn't succeed (5-2)";
  todo_eval_ok '$a.backend = Backend1.new()', "setting a handler object (5)";
  todo_eval_ok '!$a ~~ Backend1',             "object wasn't isa()ed (5)";
  todo_eval_is '$a.hi',     42, "method was successfully handled by backend object (5-1)";
  todo_eval_is '$a.cool', 1337, "method was successfully handled by backend object (5-2)";
}
