#!/usr/bin/pugs

use v6;
use Test;

=pod

The parser has difficulties with if statements whose condition is a closure.

  pugs> . if { 0 } { $foo = 2 } else { $foo = 3 }
  *** Error:
  unexpected "e"
  expecting term postfix, operator, ",", ":", adverb, postfix conditional, postfix loop, postfix iteration, ";" or end of input
    at <interactive> at line 1, column 24
  pugs>

=cut

plan 2;

# This incorrectly fails to parse...
{
	my $foo = 1;
	eval 'if { 0 } { $foo = 2 } else { $foo = 3 }';
	is $foo, 2, 'if with no parens, and closure as cond', :todo<bug>;
	### This test is copied from t/statements/if.t
};
# but curiously it parses and works if semicolons are added...
{
	my $foo = 1;
	eval 'if { 0 } { $foo = 2; } else { $foo = 3; }';
	is $foo, 2, 'if with no parens, and closure as cond';
};
