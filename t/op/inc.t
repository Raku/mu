#!/usr/bin/pugs

use v6;
require Test;

plan(15);

# Mostly copied from Perl 5.8.4 s t/op/inc.t

# Verify that addition/subtraction properly upgrade to doubles.
# These tests are only significant on machines with 32 bit longs,
# and two s complement negation, but should not fail anywhere.

my $a = 2147483647;
my $c=$a++;
ok ($a == 2147483648);

$a = 2147483647;
$c=eval '++$a';
ok ($a == 2147483648);

$a = 2147483647;
$a=$a+1;
ok ($a == 2147483648);

$a = -2147483648;
$c=$a--;
ok ($a == -2147483649);

$a = -2147483648;
$c=eval '--$a';
ok ($a == -2147483649);

$a = -2147483648;
$a=$a-1;
ok ($a == -2147483649);

$a = 2147483648;
$a = -$a;
$c=$a--;
ok ($a == -2147483649);

$a = 2147483648;
$a = -$a;
$c=eval '--$a';
ok ($a == -2147483649);

$a = 2147483648;
$a = -$a;
$a=$a-1;
ok ($a == -2147483649);

$a = 2147483648;
my $b = -$a;
$c=$b--;
ok($b == -$a-1);
# $b is rightfully -2147483649, but -$a-1 is -2147483647

$a = 2147483648;
$b = -$a;
$c=eval '--$b';
ok($b == -$a-1);

$a = 2147483648;
$b = -$a;
$b=$b-1;
ok ($b == eval '-(++$a)');

$a = undef;
ok ($a++ eq '0');

$a = undef;
ok(eval '!defined($a--)');

$a = 'x';
is($a++, 'x', 'magical ++ should not be numified');

my %a = ('a' => 1);
%a{'a'}++;
ok(%a{'a'} == 2);

my %b = ('b' => 1);
my $var = 'b';
%b{$var}++;
ok(%b{$var} == 2);

my @a = (1);
@a[1]++;
ok(@a[1] == 2);

my @b = (1);
my $moo = 1;
@b[$moo]++;
ok(@b[$moo] == 2);
