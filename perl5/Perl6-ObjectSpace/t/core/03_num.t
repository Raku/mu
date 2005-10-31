#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::Num');

# make some numbers

my $one = num->new(1);
isa_ok($one, 'num');

cmp_ok($one->to_native, '==', 1, '... got the right native value');

my $one_bit = $one->to_bit;
isa_ok($one_bit, 'bit');
cmp_ok($one_bit->to_native, '==', 1, '... got the right bit value');

my $one_str = $one->to_str;
isa_ok($one_str, 'str');
cmp_ok($one_str->to_native, 'eq', '1', '... got the right str value');

my $two = num->new(2);
isa_ok($two, 'num');

cmp_ok($two->to_native, '==', 2, '... got the right native value');

my $two_bit = $two->to_bit;
isa_ok($two_bit, 'bit');
cmp_ok($two_bit->to_native, '==', 1, '... got the right bit value');

my $two_str = $two->to_str;
isa_ok($two_str, 'str');
cmp_ok($two_str->to_native, 'eq', '2', '... got the right str value');

# do some math

# add 

my $three = $one->add($two);
isa_ok($three, 'num');

cmp_ok($three->to_native, '==', 3, '... got the right native value');

my $three_bit = $three->to_bit;
isa_ok($three_bit, 'bit');
cmp_ok($three_bit->to_native, '==', 1, '... got the right bit value');

my $three_str = $three->to_str;
isa_ok($three_str, 'str');
cmp_ok($three_str->to_native, 'eq', '3', '... got the right str value');

# multiply

my $six = $three->multiply($two);
isa_ok($six, 'num');

cmp_ok($six->to_native, '==', 6, '... got the right native value');

my $six_bit = $six->to_bit;
isa_ok($six_bit, 'bit');
cmp_ok($six_bit->to_native, '==', 1, '... got the right bit value');

my $six_str = $six->to_str;
isa_ok($six_str, 'str');
cmp_ok($six_str->to_native, 'eq', '6', '... got the right str value');

# subtract

my $four = $six->subtract($two);
isa_ok($four, 'num');

cmp_ok($four->to_native, '==', 4, '... got the right native value');

my $four_bit = $four->to_bit;
isa_ok($four_bit, 'bit');
cmp_ok($four_bit->to_native, '==', 1, '... got the right bit value');

my $four_str = $four->to_str;
isa_ok($four_str, 'str');
cmp_ok($four_str->to_native, 'eq', '4', '... got the right str value');

# divide

my $ten = $six->add($four);
isa_ok($ten, 'num');

cmp_ok($ten->to_native, '==', 10, '... got the right native value');

my $five = $ten->divide($two);
isa_ok($five, 'num');

cmp_ok($five->to_native, '==', 5, '... got the right native value');

# test some conversions

# equal to

{
    my $value = num->new(5)->equal_to(num->new(5));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

{
    my $value = num->new(5)->equal_to(num->new(6));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

# not equal to

{
    my $value = num->new(5)->not_equal_to(num->new(5));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

{
    my $value = num->new(5)->not_equal_to(num->new(10));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

# greater than

{
    my $value = num->new(5)->greater_than(num->new(5));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

{
    my $value = num->new(5)->greater_than(num->new(2));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

# greater than or equal to

{
    my $value = num->new(5)->greater_than_or_equal_to(num->new(2));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

{
    my $value = num->new(5)->greater_than_or_equal_to(num->new(5));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

{
    my $value = num->new(5)->greater_than_or_equal_to(num->new(100));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

# less than

{
    my $value = num->new(5)->less_than(num->new(5));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

{
    my $value = num->new(5)->less_than(num->new(2));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

{
    my $value = num->new(5)->less_than(num->new(50));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

# less than or equal to

{
    my $value = num->new(5)->less_than_or_equal_to(num->new(10));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

{
    my $value = num->new(5)->less_than_or_equal_to(num->new(5));
    isa_ok($value, 'bit');
    is($value, $bit::TRUE, '... our comparison is true');
}

{
    my $value = num->new(5)->less_than_or_equal_to(num->new(1));
    isa_ok($value, 'bit');
    is($value, $bit::FALSE, '... our comparison is false');
}

# TODO: do some tests with the autoconversion stuff ...

