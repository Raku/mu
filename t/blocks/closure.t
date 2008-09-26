use v6;

use Test;
plan 8;

diag "Testing for calling block bindings...";
ok eval(q[
	my &foo := { "foo" };
	foo;
]), "Calling block binding without argument. (Runtime)";

ok eval(q[
	my &foo ::= { "foo" };
	foo;
]), "Calling block binding without argument. (Compile-time)";

ok eval(q[
	my &foo := { $^a };
	foo(1);
]), "Calling block binding with argument. (Runtime, with parens)";

ok eval(q[
	my &foo := { $^a };
	foo 1;
]), "Calling block binding with argument. (Runtime, no parens)";

ok eval(q[
	my &foo ::= { $^a };
	foo(1);
]), "Calling block binding with argument. (Compile-time, with parens)";

ok eval(q[
	my &foo ::= { $^a };
	foo 1;
]), "Calling block binding with argument. (Compile-time, no parens)";


my &foo_r := { $^a + 5 };
my &foo_c ::= { $^a + 5 };
is foo_r(1), 6, "Testing the value for placeholder(Runtime binding)";
is foo_c(1), 6, "Testing the value for placeholder(Compile-time binding)";

