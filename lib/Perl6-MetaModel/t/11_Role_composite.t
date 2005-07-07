#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 27;
use Test::Exception;

use Perl6::MetaModel;

=pod

This test file demonstrates multi-level deep role hierarchies.

=cut

# roles which are 2 levels deep

role rFoo => {
    methods => {
        foo => sub { 'rFoo::foo' }
    }
};

role rBar => {
    methods => {
        bar => sub { 'rBar::bar' }
    }
};

role rFooBar => {
    does => [ 'rFoo', 'rBar' ],
    methods => {
        foo_bar => sub { 
            my $self = shift;
            ($self->foo() . '/' . $self->bar());
        }
    }
};

class FooBar => {
    does => [ 'rFooBar' ]  
};

my $foobar = FooBar->new();
isa_ok($foobar, 'FooBar');

can_ok($foobar, 'foo');
can_ok($foobar, 'bar');
can_ok($foobar, 'foo_bar');

ok($foobar->does('rFoo'), '... $foobar does rFoo');
ok($foobar->does('rBar'), '... $foobar does rBar');
ok($foobar->does('rFooBar'), '... $foobar does rFooBar');

is_deeply(
    [ $foobar->does() ],
    [ 'rFooBar', 'rFoo', 'rBar' ],
    '... got all the right roles with does()');

is($foobar->foo(), 'rFoo::foo', '... foo() returned the expected thing');
is($foobar->bar(), 'rBar::bar', '... bar() returned the expected thing');
is($foobar->foo_bar(), 'rFoo::foo/rBar::bar', '... foo_bar() returned the expected thing');

# and now 3 levels deep

role rFooBarBaz => {
    does => [ 'rFooBar' ],
    methods => {
        baz => sub { 'rFooBarBaz::baz' }
    }
};

class FooBarBaz => {
    does => [ 'rFooBarBaz' ]  
};

my $foobarbaz = FooBarBaz->new();
isa_ok($foobarbaz, 'FooBarBaz');

can_ok($foobarbaz, 'foo');
can_ok($foobarbaz, 'bar');
can_ok($foobarbaz, 'foo_bar');
can_ok($foobarbaz, 'baz');

ok($foobarbaz->does('rFoo'), '... $foobar does rFoo');
ok($foobarbaz->does('rBar'), '... $foobar does rBar');
ok($foobarbaz->does('rFooBar'), '... $foobar does rFooBar');
ok($foobarbaz->does('rFooBarBaz'), '... $foobar does rFooBarBaz');

is_deeply(
    [ $foobarbaz->does() ],
    [ 'rFooBarBaz', 'rFooBar', 'rFoo', 'rBar' ],
    '... got all the right roles with does()');

is($foobarbaz->foo(), 'rFoo::foo', '... foo() returned the expected thing');
is($foobarbaz->bar(), 'rBar::bar', '... bar() returned the expected thing');
is($foobarbaz->foo_bar(), 'rFoo::foo/rBar::bar', '... foo_bar() returned the expected thing');
is($foobarbaz->baz(), 'rFooBarBaz::baz', '... baz() returned the expected thing');

# conflicts with 2 levels deep

role rFoo2 => {
    methods => {
        foo => sub { 'Foo2::foo' }
    }  
};

dies_ok {
    class ThisFails => {
        does => [ 'rFooBar', 'rFoo2' ]
    }    
} '... we got an error because rFooBar::foo will conflict with rFoo2::foo';

# conflicts 3 levels deep

dies_ok {
    class ThisFailsToo => {
        does => [ 'rFooBarBaz', 'rFoo2' ]
    }    
} '... we got an error because rFooBarBaz::foo will conflict with rFoo2::foo';








