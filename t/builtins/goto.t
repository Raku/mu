#!/usr/bin/pugs

use v6;
require Test;

=kwid

Tests for the goto() builtin

We have "phases" to make sure the gotos didn't run wild.

=cut

plan 2;

our $phase;

sub test1_ok { return 1 }
sub test1 {
	&test1_ok.goto();
	return;
}
ok(test1(), "&sub.goto does");
is(++$phase, 1, "phase completed");

# the same, but with subs declared after the call.

sub test2 {
	&test2_ok.goto();
	return;
}
sub test2_ok { pass("&sub.goto does (forward reference)") }
ok(test2(), "&sub.goto does (forward reference)");
is(++$phase, 2, "phase completed");

eval_ok('test3()', "&sub.goto does (real forward reference)");
sub test3 {
   &test3_ok.goto();
   return; # fail
}
sub test3_ok { 1 }
is(++$phase, 3, "phase completed");
