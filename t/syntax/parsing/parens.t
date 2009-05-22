use v6;

use Test;

=begin pod

parens appear to be eating spaces after them

  pugs> . (1) { say }
  Noop;
  Syn "{}" {1;
            App &say ()}

=end pod

plan 1;

# This should fail to parse but doesn't
eval_dies_ok '(1) { $foo = 2 }', 'parens do not eat spaces after them';

# vim: ft=perl6
