#!/usr/bin/pugs

use v6;
require Test;

=kwid

Tests for the goto() builtin

We have "phases" to make sure the gotos didn't run wild.

=cut

plan 6;

our $phase;

sub test1_ok { return 1 }
sub test1 {
	&test1_ok.goto();
	return 0;
}
ok(test1(), "&sub.goto does");
is(++$phase, 1, "phase completed");

# the same, but with subs declared after the call.

sub test2 {
	&test2_ok.goto();
	return 0;
}
sub test2_ok { return 1 }
ok(test2(), "&sub.goto does (forward reference)");
is(++$phase, 2, "phase completed");

todo_eval_ok('test3()', "&sub.goto does (real forward reference)");
sub test3 {
   &test3_ok.goto();
   return 0;
}
sub test3_ok { 1 }
is(++$phase, 3, "phase completed");
