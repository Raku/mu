#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Exception;

use Perl6::MetaModel;

my $Foo = class 'Foo' => {
    'is' => [ $::Object ],    
    'class_methods' => {
        'this_will_not_die' => sub { $::CLASS }
    },
    'methods' => {
        'bar' => sub { $::CLASS->class::this_will_not_die },
        'foo' => sub { $::CLASS }
    }
};

{
    my $val;
    lives_ok {
        $val = $Foo->class::this_will_not_die();
    } '... $::CLASS can be called from a Class method';
    cmp_ok($val, '==', $Foo, '... we got the class object back');
}

my $foo = $Foo->new();
isa_ok($foo, 'Foo');
isa_ok($foo, 'Object');

{
    my $val;
    lives_ok {
        $val = $foo->bar();
    } '... $::CLASS can be used to call Class methods';
    cmp_ok($val, '==', $Foo, '... we got the class object back');    
}

{
    my $val;
    lives_ok {
        $val = $foo->foo();
    } '... $::CLASS can be called from an instance method';
    cmp_ok($val, '==', $Foo, '... we got the class object back');    
}
