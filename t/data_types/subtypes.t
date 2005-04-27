#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=pod

Tests subtypes, specifically in the context of multimethod dispatch.

L<S12/"Types and Subtypes">

=cut

my $abs = '
multi sub my_abs (Num where { $^n >= 0 } $n){ $n }
multi sub my_abs (Num where { $^n <  0 } $n){ -$n }
';

eval_ok("$abs; 1", "we can compile subtype declarations", :todo);

eval_is("$abs; my_abs(3)", 3, "and we can use them, too", :todo);
eval_is("$abs; my_abs(-5)", 5, "and they actually work", :todo);

