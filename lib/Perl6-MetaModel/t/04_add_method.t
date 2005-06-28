#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::MetaModel;
use Perl6::Method;

=pod

This test file demonstrates the ability to add methods to a class 
which are then accessible from an already created instance.

=cut

class Foo => {
    class => {
        methods => {
            'foo' => sub { 'FOO' }
        }
    }
};

my $foo = Foo->new_instance();
isa_ok($foo, 'Foo');

can_ok($foo, 'foo');
is($foo->foo(), 'FOO', '... $foo->foo() works');

ok(!$foo->can('bar'), '... $foo cannot bar() yet');

Foo->class->metaclass->add_method('bar' => Perl6::Method->new('Foo' => sub { 'BAR' }));

can_ok($foo, 'bar');
is($foo->bar(), 'BAR', '... $foo->bar() works');