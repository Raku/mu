#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Method;

=pod

This test file demonstrates the ability to add methods to a class 
which are then accessible from an already created instance.

=cut

class Foo => {
    is => [ 'Perl6::Object' ],
    instance => {
        methods => {
            'foo' => sub { 'FOO' }
        }
    }
};

my $foo = Foo->new();
isa_ok($foo, 'Foo');

can_ok($foo, 'foo');
is($foo->foo(), 'FOO', '... $foo->foo() works');

ok(!$foo->can('bar'), '... $foo cannot bar() yet');

::dispatch(Foo->meta, 'add_method', 0, ('bar' => Perl6::Instance::Method->new('Foo' => sub { 'BAR' })));

can_ok($foo, 'bar');
is($foo->bar(), 'BAR', '... $foo->bar() works');

::dispatch(Perl6::Object->meta, 'add_method', 0, ('a_method' => Perl6::Instance::Method->new('Perl6::Object' => sub { 'Perl6::Object::a_method' })));

can_ok($foo, 'a_method');
is($foo->a_method(), 'Perl6::Object::a_method', '... $foo->a_method() works ("a_method" was added to Perl6::Object)');


