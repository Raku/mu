#!/usr/bin/pugs

# Checking that testing is sane: use

use v6;

say '1..2';

use Test;

my $x = '0';
ok $x == $x;

say 'ok 2';
