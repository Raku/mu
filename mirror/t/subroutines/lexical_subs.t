#!/usr/bin/pugs

use v6;
use Test;

plan 1;

# my sub's stopped working between 4070 and 4083.
# see also t/statements/statement_modifiers.t
sub f() { my sub g(){"g"}; my sub h(){g()}; h()};
is(f(),'g');
