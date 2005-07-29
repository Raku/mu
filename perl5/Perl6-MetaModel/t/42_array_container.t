#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 17;

use Perl6::Container::Array;
use Perl6::Container::Scalar;

my $a = Perl6::Container::Array->new;

isa_ok($a, 'Perl6::Container::Array');

is($a->FETCH_SIZE, 0, "... array size is 0");
is_deeply([ $a->FETCH_KEYS ], [], "... no keys in array");
is_deeply($a->FETCH, [], "... no elements in array");

$a->PUSH([21]);

is($a->FETCH_SIZE, 1, "... array size is 1");
is_deeply([ $a->FETCH_KEYS ], [ 0 ], "... only key is '0'");
is_deeply($a->FETCH, [ Perl6::Container::Scalar->new()->STORE(21) ], "... one element in array");

my $s = Perl6::Container::Scalar->new->STORE(42);
$a->STORE_ELEM(1, $s);

is($a->FETCH_SIZE, 2, "... array size is 2");
is_deeply([ $a->FETCH_KEYS ], [ 0, 1 ], "... two keys");
is_deeply($a->FETCH, [ Perl6::Container::Scalar->new()->STORE(21), $s ], "... two elements in array");

is($a->FETCH_VAL(0), 21, "... first value");
is($a->FETCH_VAL(1), 42, "... second value");

$a->STORE_VAL(3, 84);
is($a->FETCH_SIZE, 4, "... array size is 4");
is($a->FETCH_ELEM(3)->FETCH, 84, "... scalar container returns value");
is_deeply(
    $a->FETCH, 
    [ 
        Perl6::Container::Scalar->new()->STORE(21), 
        $s,
        Perl6::Container::Scalar->new()->STORE(undef),         
        Perl6::Container::Scalar->new()->STORE(84),         
    ], 
    "... four elements in array");
    
is($a->FETCH_VAL(2), undef, "... third value");
is($a->FETCH_VAL(3), 84, "... fourth value");    

