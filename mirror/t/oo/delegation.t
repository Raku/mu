#!/usr/bin/pugs

use v6;
use Test;

plan 34;

=pod

Delegation tests from L<S12/"Delegation">

=cut

# L<S12/"Delegation">

ok eval('
  class Backend1 { method hi() { 42 } method cool() { 1337 } }
  class Backend2 { method hi() { 23 } method cool() {  539 } }
  class Frontend { has $.backend is rw handles "hi" }
  1
'), "class definition worked";

is eval('Backend1.new.hi'), 42, "basic sanity (1)", :todo<feature>;
is eval('Backend2.new.hi'), 23, "basic sanity (2)", :todo<feature>;

{
  my $a;
  ok eval('$a = Frontend.new'), "basic instantiation worked (1)", :todo<feature>;
  ok eval('!try { $a.hi }'), "calling a method on no object didn't succeed (1)";
  ok eval('$a.backend = Backend1.new()'), "setting a handler object (1)", :todo<feature>;
  ok eval('!$a ~~ Backend1'),             "object wasn't isa()ed (1)", :todo<feature>;
  is eval('$a.hi'), 42, "method was successfully handled by backend object (1)", :todo<feature>;
}

{
  my $a;
  ok eval('$a = Frontend.new'), "basic instantiation worked (2)", :todo<feature>;
  ok eval('!try { $a.hi }'), "calling a method on no object didn't succeed (2)";
  ok eval('$a.backend = Backend2.new()'), "setting a handler object (2)", :todo<feature>;
  ok eval('!$a ~~ Backend2'),             "object wasn't isa()ed (2)", :todo<feature>;
  is eval('$a.hi'), 23, "method was successfully handled by backend object (2)", :todo<feature>;
}


# L<S12/"Delegation" /Any other kind of argument to handles is considered to be a smartmatch selector for methods/>
ok eval('class ReFrontend { has $.backend is rw handles /^hi/ }; 1'),
  "class definition using a smartmatch handle worked";
{
  my $a;
  ok eval('$a = ReFrontend.new'), "basic instantiation worked (3)", :todo<feature>;
  ok eval('!try { $a.hi }'), "calling a method on no object didn't succeed (3)";
  ok eval('$a.backend = Backend1.new()'), "setting a handler object (3)", :todo<feature>;
  ok eval('!$a ~~ Backend1'),             "object wasn't isa()ed (3)", :todo<feature>;
  is eval('$a.hi'), 42, "method was successfully handled by backend object (3)", :todo<feature>;
}


# L<S12/"Delegation" /If you say/>
ok eval('class ClassFrontend { has $.backend is rw handles Backend2 }; 1'),
  "class definition using a Class handle worked";
{
  my $a;
  ok eval('$a = ClassFrontend.new'), "basic instantiation worked (4)", :todo<feature>;
  ok eval('!try { $a.hi }'), "calling a method on no object didn't succeed (4)";
  ok eval('$a.backend = Backend1.new()'), "setting a handler object (4)", :todo<feature>;
  ok eval('!$a ~~ Backend1'),             "object wasn't isa()ed (4-1)", :todo<feature>;
  ok eval('!$a ~~ Backend2'),             "object wasn't isa()ed (4-2)", :todo<feature>;
  is eval('$a.hi'), 42, "method was successfully handled by backend object (4)", :todo<feature>;
}


# L<S12/"Delegation" /You can specify multiple method names:/>
ok eval('class MultiFrontend { has $.backend is rw handles <hi cool> }; 1'),
  "class definition using multiple method names worked";
{
  my $a;
  ok eval('$a = MultiFrontend.new'), "basic instantiation worked (5)", :todo<feature>;
  ok eval('!try { $a.hi   }'), "calling a method on no object didn't succeed (5-1)";
  ok eval('!try { $a.cool }'), "calling a method on no object didn't succeed (5-2)";
  ok eval('$a.backend = Backend1.new()'), "setting a handler object (5)", :todo<feature>;
  ok eval('!$a ~~ Backend1'),             "object wasn't isa()ed (5)", :todo<feature>;
  is eval('$a.hi'),     42, "method was successfully handled by backend object (5-1)", :todo<feature>;
  is eval('$a.cool'), 1337, "method was successfully handled by backend object (5-2)", :todo<feature>;
}
