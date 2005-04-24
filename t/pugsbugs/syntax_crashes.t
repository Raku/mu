#!/usr/bin/pugs
use v6;
require Test;

plan 1;

my $junc = 1|2|3;
ok $junc.pick == 1|2|3;

# Note:
#   ok 1|2|3 == $junc.pick;
# works fine, for some strange reason.