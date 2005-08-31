#!/usr/bin/pugs

use v6;
use Test;

=kwid

basic Number tests

=cut

plan 44;

my $a = 1; "$a";
isa_ok($a, 'Int');
is($a, "1", '1 stringification works');

my $a = -1; "$a";
isa_ok($a, 'Int');
is($a, "-1", '-1 stringification works');

my $a = 1.; "$a";
isa_ok($a, 'Rat');
is($a, "1", '1. stringification works');

my $a = -1.; "$a";
isa_ok($a, 'Rat');
is($a, "-1", '-1 stringification works');

my $a = 0.1; "$a";
isa_ok($a, 'Rat');
is($a, "0.1", '0.1 stringification works');

my $a = -0.1; "$a";
isa_ok($a, 'Rat');
is($a, "-0.1", '-0.1 stringification works');

my $a = 10.01; "$a";
isa_ok($a, 'Rat');
is($a, "10.01", '10.01 stringification works');

my $a = 1e3; "$a";
isa_ok($a, 'Rat');
is($a, "1000", '1e3 stringification works');

my $a = 10.01e3; "$a";
isa_ok($a, 'Rat');
is($a, "10010", '10.01e3 stringification works');

my $a = 0b100; "$a";
isa_ok($a, 'Int');
is($a, "4", '0b100 (binary) stringification works');

my $a = 0x100; "$a";
isa_ok($a, 'Int');
is($a, "256", '0x100 (hex) stringification works');

my $a = 0o100; "$a";
isa_ok($a, 'Int');
is($a, "64", '0o100 (octal) stringification works');

my $a = 1; "$a"; 
is($a + 1, 2, 'basic addition works');

my $a = -1; "$a";
ok($a + 1 == 0, 'basic addition with negative numbers works'); # parsing bug

my $a = 80000.0000000000000000000000000;
isa_ok($a, 'Rat');
ok($a == 80000.0, 'trailing zeros compare correctly');

my $a = 1.0000000000000000000000000000000000000000000000000000000000000000000e1;
isa_ok($a, 'Rat');
ok($a == 10.0, 'trailing zeros compare correctly');

my $a = "1.01";
isa_ok(+$a, "Num");
is(+$a, 1.01, "1.01 numifies to 1.01");

my $a = "0d01.01";
isa_ok(+$a, "Num");
is(+$a, 1.01, "0d01.01 numifies to 1.01", :todo<bug>);

my $a = "1.01";
isa_ok(int($a), "Int");
is(int($a), 1, "1.01 intifies to 1");

my $a = "0d0101";
isa_ok(+$a, "Num");
is(+$a, 101, "0d0101 numifies to 101");

my $a = 2 ** 65; # over the 64 bit limit too
is($a, 36893488147419103232, "we have bignums, not weeny floats");

eval_is('42_000', 42000, 'underscores allowed (and ignored) in numeric literals');
eval_is('42_127_000', 42127000, 'multiple underscores ok');
eval_is('42.0_1', 42.01, 'underscores in fraction ok');
eval_is('4_2.01', 42.01, 'underscores in whole part ok');
eval_is('4_2__.__0_1___', 42.01, 'lots of underscores are ok', :todo);

