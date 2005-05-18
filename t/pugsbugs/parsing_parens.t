#!/usr/bin/pugs

use v6;
use Test;

=pod

parens appear to be eating spaces after them

  pugs> . (1) { say }
  Noop;
  Syn "{}" {1;
            App &say ()}

=cut

plan 1;

# This should fail to parse but doesn't
{
	my $foo = 1;
	eval '(1) { $foo = 2 }';
	is $foo, 1, 'checking that parens do not eat spaces after them', :todo<bug>;
};
