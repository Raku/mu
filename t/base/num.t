#!/usr/bin/pugs

use v6;
require Test;

plan 16;

my $a = 1; "$a";
is($a, "1", '1 stringification works');

my $a = -1; "$a";
is($a, "-1", '-1 stringification works');

my $a = 1.; "$a";
is($a, "1", '1. stringification works');

my $a = -1.; "$a";
is($a, "-1", '-1 stringification works');

my $a = 0.1; "$a";
is($a, "0.1", '0.1 stringification works');

my $a = -0.1; "$a";
is($a, "-0.1", '-0.1 stringification works');

my $a = 10.01; "$a";
is($a, "10.01", '10.01 stringification works');

my $a = 1e3; "$a";
is($a, "1000", '1e3 stringification works');

my $a = 10.01e3; "$a";
is($a, "10010", '10.01e3 stringification works');

my $a = 0b100; "$a";
is($a, "4", '0b100 (binary) stringification works');

my $a = 0x100; "$a";
is($a, "256", '0x100 (hex) stringification works');

my $a = 0o100; "$a";
is($a, "64", '0o100 (octal) stringification works');

my $a = 1; "$a"; 
is($a + 1, 2, 'basic addition works');

my $a = -1; "$a";
ok($a + 1 == 0, 'basic addition with negative numbers works'); # parsing bug

my $a = 80000.0000000000000000000000000;
ok($a == 80000.0, 'trailing zeros compare correctly');

my $a = 1.0000000000000000000000000000000000000000000000000000000000000000000e1;
ok($a == 10.0, 'trailing zeros compare correctly');
