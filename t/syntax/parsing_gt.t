#!/usr/bin/pugs

use v6;
use Test;

BEGIN { plan 3 }

# This parsefail is really weird...
# I'm sorry I couldn't track down this bug further -- it's extremely weird.
# The following two tests are from rand.t. Isolated, they parse and work as
# expected.  But when the additional commands are added, Pugs dies at
# compilation time:
#   *** Error: 
#   unexpected "("
#   expecting word character, "::", ".", operator, ",", ":", ")", block construct, term postfix, postfix conditional, postfix loop, postfix iteration, ";" or end of input at t/pugsbugs/parsing_gt.t line 13, column 3
# First, I thought it was only a predence issue, so I tried adding parens etc.,
# but that didn't help.
# Now the *really* evil part: Even if you comment the last line, the test still
# keeps parsefailing!

ok(rand() >= 0, 'random numbers are greater than or equal to 0');
ok(rand() < 1, 'random numbers are less than 1');

ok 3 > 0, "3 is greater than 0";

# Now, that Autrijus has fixed the bug, the explanation is simple, too:
# The thing got parsed as
#   rand()< 1
# (hash subscript), and then, of course, it had to die, as there was no
# matching >.
