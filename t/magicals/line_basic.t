#!/usr/bin/pugs

use v6;
use Test;

plan 7;

=kwid

Basic tests for #line

Current status: the last subtest fails and the ones before it pass.
But when you *delete* $last_subtest, the new $last_subtest passes.

=cut

is($?POSITION, "$?FILE at line 17, column 1", 'plain old $?POSITION');

my $dummy = 0; # This comment is not at column1
$dummy    = 1; # And neither is this one.

is($?POSITION, "$?FILE at line 22, column 1", "plain comments don't disrupt position");

# This comment does start at column1

is($?POSITION, "$?FILE at line 26, column 1", "comments at column1 don't disrupt position");

#line 1024
is($?POSITION, "$?FILE at line 1024, column 1", "basic #line works");
