#!/usr/bin/pugs

use v6;
require Test;

plan 15;

# ref() on basic types

my $a;
is(ref($a), 'Any', 'it is an Any type');

my @a;
is(ref(@a), 'Array', 'it is an Array type');

my %a;
is(ref(%a), 'Hash', 'it is an Hash type');

# ref() on reference types

my $b1 = [];
is(ref($b1), 'List', 'it is a List ref type');

# this seems to be the only way to make a hash - ref at the moment
my %b2 = ("one", 1); my $b2 = %b2;
is(ref($b2), 'Hash', 'it is a Hash ref type'); 

# ref() on subroutines

my $s1 = sub {};
is(ref($s1), 'Sub', 'it is a Sub type');

my $s2 = {};
is(ref($s2), 'Sub', 'it is a Sub type (bare block)');

my $s3 = -> {};
is(ref($s3), 'Sub', 'it is a Sub type (pointy sub)');

# ref() on different types of scalars

my $int = 0;
is(ref($int), 'Int', 'it is an Int type');

# the only way I can seem to get Num is to force numeric context
my $num = '';
is(ref(+$num), 'Num', 'it is an Num type');

my $float = 0.5;
is(ref($float), 'Rat', 'it is an Rat type');

my $string = "Hello World";
is(ref($string), 'Str', 'it is a Str type');

my $bool = (0 == 0);
is(ref($bool), 'Bool', 'it is a Bool type');

my $pair = "foo" => "bar";
is(ref($pair), 'Pair', 'it is a Pair type');

my $rule = rx:perl5{^hello\sworld$};
is(ref($rule), 'Rule', 'it is a Rule type');
