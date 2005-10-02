#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;
use Test::Exception;

=pod

This is one possible solution for supporting singleton methods, using
Roles. It is not ideal by far since it uses AUTOLOAD to capture any
singleton method calls.

I had attempted to subclass ::Class to solve this issue, but that presented
one major problem, which is that when a method is called it only deals with 
the class of an instance, and there is no easy way to involve the instance
itself in that dispatch.

=cut

use Perl6::MetaModel;

my $WithSingletonMethod = role 'WithSingletonMethod' => {
    attributes => [ '%:singleton_methods' ],
    methods => {
        'add_singleton_method' => sub {
            my ($self, $label, $method) = @_;
            _('%:singleton_methods')->{$label} = $method;
        },
        'AUTOLOAD' => sub {
            my $label = $::CLASS->FETCH('$AUTOLOAD');            
            return _('%:singleton_methods')->{$label}->(@_) 
                if exists _('%:singleton_methods')->{$label};
            die "Method not found";
        }
    }
};

my $Foo = class 'Foo' => {
    is => [ $::Object ],
    does => [ $WithSingletonMethod ],
    methods => {
        'bar' => sub { 'Foo::bar' }
    }
};

my $foo = $Foo->new();
my $foo2 = $Foo->new();

is($foo->bar(), 'Foo::bar', '... normal methods work');
is($foo2->bar(), 'Foo::bar', '... normal methods work');

lives_ok {
    $foo->add_singleton_method('baz' => sub { '$foo::baz' });
} '... added singleton method okay';

is($foo->baz(), '$foo::baz', '... singleton methods work');

dies_ok {
    $foo2->baz()
} '... but singleton methods are not shared across instances';
