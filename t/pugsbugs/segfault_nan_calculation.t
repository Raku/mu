#!/usr/bin/pugs

use v6;
require Test;

plan(1);

# crashes Pugs
# GHC bug?

#my $nan1 = NaN**NaN;
#ok($nan1 == NaN, "NaN**NaN works and doesn't segfault");

fail("FIXME parsefail");