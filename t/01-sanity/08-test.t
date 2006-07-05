use v6-pugs;

# Checking that testing is sane: Test.pm

use Test;

plan 1;

my $x = '0';
ok $x == $x;
