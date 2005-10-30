#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::List');

my $list = list->new();
isa_ok($list, 'list');

{
    is_deeply(
        [ $list->to_native ],
        [],
        '... got the right native type');
    
    my $length = $list->length;
    isa_ok($length, 'num');
    cmp_ok($length->to_native, '==', 0, '... got the right numeric length');
    
    my $elems = $list->elems;
    isa_ok($elems, 'num');
    cmp_ok($elems->to_native, '==', -1, '... got the right numeric elems');    
    
    my $list_num = $list->to_num;
    isa_ok($list_num, 'num');
    cmp_ok($list_num->to_native, '==', 0, '... got the right numeric value');    
    
    my $list_bit = $list->to_bit;
    isa_ok($list_bit, 'bit');    
    cmp_ok($list_bit->to_native, '==', 0, '... got the right bit value');     
    
    my $list_str = $list->to_str;
    isa_ok($list_str, 'str');    
    cmp_ok($list_str->to_native, 'eq', '', '... got the right string value');  
    
    isa_ok($list->fetch(num->new(0)), 'nil');
    isa_ok($list->fetch(num->new(100)), 'nil');    
    
    my $join_list = $list->join(str->new(', '));
    isa_ok($join_list, 'str');    
    cmp_ok($join_list->to_native, 'eq', '', '... got the right joined value');      
}

my $at_zero = str->new('hello world');
isa_ok($at_zero, 'str');

$list->store(num->new(0), $at_zero);

{
    is_deeply(
        [ $list->to_native ],
        [ $at_zero ],
        '... got the right native type');

    my $length = $list->length;
    isa_ok($length, 'num');
    cmp_ok($length->to_native, '==', 1, '... got the right numeric length');
    
    my $elems = $list->elems;
    isa_ok($elems, 'num');
    cmp_ok($elems->to_native, '==', 0, '... got the right numeric elems');        

    my $list_num = $list->to_num;
    isa_ok($list_num, 'num');
    cmp_ok($list_num->to_native, '==', 1, '... got the right numeric value');    

    my $list_bit = $list->to_bit;
    isa_ok($list_bit, 'bit');    
    cmp_ok($list_bit->to_native, '==', 1, '... got the right bit value');     

    my $list_str = $list->to_str;
    isa_ok($list_str, 'str');    
    cmp_ok($list_str->to_native, 'eq', 'hello world', '... got the right string value');  

    isa_ok($list->fetch(num->new(0)), 'str');
    is($list->fetch(num->new(0)), $at_zero, '... got the right value at 0');
    
    isa_ok($list->fetch(num->new(100)), 'nil');    
    
    my $join_list = $list->join(str->new(', '));
    isa_ok($join_list, 'str');    
    cmp_ok($join_list->to_native, 'eq', 'hello world', '... got the right joined value');          
}

my $at_four = str->new('number four');
isa_ok($at_four, 'str');

$list->store(num->new(4), $at_four);

{
    is_deeply(
        [ $list->to_native ],
        [ $at_zero, undef, undef, undef, $at_four ],
        '... got the right native type');

    my $length = $list->length;
    isa_ok($length, 'num');
    cmp_ok($length->to_native, '==', 5, '... got the right numeric length');
    
    my $elems = $list->elems;
    isa_ok($elems, 'num');
    cmp_ok($elems->to_native, '==', 4, '... got the right numeric elems');        

    my $list_num = $list->to_num;
    isa_ok($list_num, 'num');
    cmp_ok($list_num->to_native, '==', 5, '... got the right numeric value');    

    my $list_bit = $list->to_bit;
    isa_ok($list_bit, 'bit');    
    cmp_ok($list_bit->to_native, '==', 1, '... got the right bit value');     

    my $list_str = $list->to_str;
    isa_ok($list_str, 'str');    
    cmp_ok($list_str->to_native, 'eq', 'hello worldnumber four', '... got the right string value');  

    isa_ok($list->fetch(num->new(0)), 'str');
    is($list->fetch(num->new(0)), $at_zero, '... got the right value at 0');
    
    isa_ok($list->fetch(num->new(1)), 'nil');     
    isa_ok($list->fetch(num->new(2)), 'nil');     
    isa_ok($list->fetch(num->new(3)), 'nil');             
    
    isa_ok($list->fetch(num->new(4)), 'str');
    is($list->fetch(num->new(4)), $at_four, '... got the right value at 4');    

    isa_ok($list->fetch(num->new(100)), 'nil');    
    
    my $join_list = $list->join(str->new(', '));
    isa_ok($join_list, 'str');    
    cmp_ok($join_list->to_native, 'eq', 'hello world, , , , number four', '... got the right joined value');          
}
