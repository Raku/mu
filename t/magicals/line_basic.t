#!/usr/bin/pugs

use v6;
use Test;

plan 4;

=kwid

Basic tests for #line

=cut

is($?POSITION, "$?FILE line 14, column 4-14", 'plain old $?POSITION');

my $dummy = 0; # This comment is not column1
$dummy    = 1; # And neither is this one.

is($?POSITION, "$?FILE line 19, column 4-14", "plain comments don't disrupt position");

# This comment does start column1

is($?POSITION, "$?FILE line 23, column 4-14", "comments column1 don't disrupt position");

#line 1024
is($?POSITION, "$?FILE line 1024, column 4-14", "basic #line works");
