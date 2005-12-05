#!/usr/bin/pugs

use v6;
use Test;

plan 5;

=kwid

<perlop/"Quote and Quote-like Operators"> # search for SMILEY

=cut

is("\x20", ' ', '\x20 normal space');
is("\xa0", ' ', '\xa0 non-breaking space');
is("\x{20}", ' ', '\x{20} normal space', :todo);
flunk('FIXME parsefail \x{a0}', :todo);
#eval_is("\x{a0}", ' ', '\x{a0} non-breaking space');
flunk('FIXME parsefail \x{263a}', :todo);
#eval_is("\x{263a}", '☺', '\x{263a} wide hex character (SMILEY)');
