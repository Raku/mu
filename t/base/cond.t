#!/usr/bin/pugs

use v6;
require Test;

=pod

Make sure conditional operators work

=cut

plan 2;

my $x = '0';

ok $x eq $x, "check that eq works";
ok $x == $x, "check that == works";

