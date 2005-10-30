#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::Core::Str');

# hello world string

my $hello = str->new('hello ');
isa_ok($hello, 'str');

is($hello->to_native, 'hello ', '... got the right string value');

my $world = str->new('world');
isa_ok($world, 'str');

is($world->to_native, 'world', '... got the right string value');

my $hello_world = $hello->concat($world);
isa_ok($hello_world, 'str');

is($hello_world->to_native, 'hello world', '... got the right string value');

my $hello_world_num = $hello_world->to_num;
isa_ok($hello_world_num, 'num');

cmp_ok($hello_world_num->to_native, '==', 0, '... got the right num');

my $hello_world_bit = $hello_world->to_bit;
isa_ok($hello_world_bit, 'bit');

cmp_ok($hello_world_bit->to_native, '==', 1, '... got the right bit');

is($hello_world->to_str, $hello_world, '... to_str returns itself');

# empty string

my $empty_string = str->new();
isa_ok($empty_string, 'str');

is($empty_string->to_native, '', '... got the right string value');

my $empty_string_num = $empty_string->to_num;
isa_ok($empty_string_num, 'num');

cmp_ok($empty_string_num->to_native, '==', 0, '... got the right num');

my $empty_string_bit = $empty_string->to_bit;
isa_ok($empty_string_bit, 'bit');

cmp_ok($empty_string_bit->to_native, '==', 0, '... got the right bit');

is($empty_string->to_str, $empty_string, '... to_str returns itself');

# concat with other things

my $num_2 = num->new(2);
isa_ok($num_2, 'num');

my $hello_world_2 = $hello_world->concat($num_2);
isa_ok($hello_world_2, 'str');

is($hello_world_2->to_native, 'hello world2', '... got the right string value');

my $on = bit->new(1);
isa_ok($on, 'bit');

my $hello_world_2_and_on = $hello_world_2->concat($on);
isa_ok($hello_world_2_and_on, 'str');

is($hello_world_2_and_on->to_native, 'hello world21', '... got the right string value');

# test some conversions

# equal to

{
    my $value = str->new('a')->equal_to(str->new('a'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

{
    my $value = str->new('a')->equal_to(str->new('f'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

# not equal to

{
    my $value = str->new('a')->not_equal_to(str->new('a'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

{
    my $value = str->new('a')->not_equal_to(str->new('g'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

# greater than

{
    my $value = str->new('a')->greater_than(str->new('a'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

{
    my $value = str->new('m')->greater_than(str->new('a'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

# greater than or equal to

{
    my $value = str->new('d')->greater_than_or_equal_to(str->new('b'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

{
    my $value = str->new('e')->greater_than_or_equal_to(str->new('e'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

{
    my $value = str->new('e')->greater_than_or_equal_to(str->new('x'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

# less than

{
    my $value = str->new('e')->less_than(str->new('e'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

{
    my $value = str->new('e')->less_than(str->new('b'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

{
    my $value = str->new('e')->less_than(str->new('z'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

# less than or equal to

{
    my $value = str->new('e')->less_than_or_equal_to(str->new('k'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

{
    my $value = str->new('e')->less_than_or_equal_to(str->new('e'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 1, '... our comparison is true');
}

{
    my $value = str->new('e')->less_than_or_equal_to(str->new('a'));
    isa_ok($value, 'bit');
    cmp_ok($value->to_native, '==', 0, '... our comparison is false');
}

