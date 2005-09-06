#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;

use Perl6::MetaModel;

=pod

This test file demonstrates the ability to add methods to a class 
which are then accessible from an already created instance.

=cut

my $Foo = class 'Foo' => {
    'is' => [ $::Object ],
    'methods' => {
        'foo' => sub { 'FOO' }
    }
};

my $foo = $Foo->new();
isa_ok($foo, 'Foo');
isa_ok($foo, 'Object');

can_ok($foo, 'foo');
is($foo->foo(), 'FOO', '... $foo->foo() works');

ok(!$foo->can('bar'), '... $foo cannot bar() yet');

$Foo->add_method('bar' => ::make_method(sub { 'BAR' }, $Foo));

can_ok($foo, 'bar');
is($foo->bar(), 'BAR', '... $foo->bar() works');

$::Object->add_method('a_method' => ::make_method(sub { 'Perl6::Object::a_method' }, $::Object));

can_ok($foo, 'a_method');
is($foo->a_method(), 'Perl6::Object::a_method', '... $foo->a_method() works ("a_method" was added to $::Object)');


