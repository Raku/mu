#!/usr/bin/pugs

use v6;
use Test;

=pod

In global substitutions, lookbehind information is lost, so some
patterns with lookbehind yield incorrect results.
See Prim/Match.hs line 129, r7551.

=cut

plan 1;

my $s="Foo Hee";
$s ~~ s:perl5:g/(?<!\:)\b(?=[A-Z])/X::/;
is($s,"X::Foo X::Hee", :todo<bug>);
# The current result of "X::Foo X::X::Hee" is obviously not correct.
