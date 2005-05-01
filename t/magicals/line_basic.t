#!/usr/bin/pugs

use v6;
use Test;

plan 11;

=kwid

Basic tests for #line

=cut

my $file = $?FILE;

is($?POSITION, "$file line 16, column 4-14", 'plain old $?POSITION');

my $dummy = 0; # This comment is not column1
$dummy    = 1; # And neither is this one.

is($?POSITION, "$file line 21, column 4-14", "plain comments don't disrupt position");

# This comment does start column1

is($?POSITION, "$file line 25, column 4-14", "comments column1 don't disrupt position");

#line 1024
is($?POSITION, "$file line 1024, column 4-14", "basic #line works");

#line1
is($?POSITION, "$file line 1027, column 4-14", "#line1 (no whitespace) is ignored");

#line1 1
is($?POSITION, "$file line 1030, column 4-14", "#line1 1 is ignored");

#line 2048 "oneword"
is($?POSITION, "oneword line 2048, column 4-14", '#line n "filename"');

#line 4096 two words
is($?POSITION, "oneword line 2050, column 4-14", '#line n two words ignored (should be quoted)');

#line 8192 unquoted
is($?POSITION, "unquoted line 8192, column 4-14", '#line n unquoted (one word is ok)');

#line 16384 123
is($?POSITION, "123 line 16382, column 4-14", '#line n unquoted-n (word can be number, too)');

#line 32768 "now is the time"
is($?POSITION, "now is the time line 32768, column 4-14", '#line n "long quoted name"');
