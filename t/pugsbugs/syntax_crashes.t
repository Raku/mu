#!/usr/bin/pugs
use v6;
require Test;

plan 2;

fail("FIXME parsefail", :todo(1));
#ok(not eval 'my @a = (1); @a<0>')   # eval should return undef on syntax error

my $junc = 1|2|3;
#ok $junc.pick == 1|2|3;
fail "FIXME parsefail", :todo(1);
# Note:
#   ok 1|2|3 == $junc.pick;
# works fine, for some strange reason.
