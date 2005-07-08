#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;

use Perl6::MetaModel;

=pod

This test file is testing the SUPER call.

=cut

class FooBase => {
    instance => {
        methods => {
            foo => sub { 'FooBase::foo' },
            bar => sub { shift; 'FooBase::bar->(' . (join ', ' => @_) . ')' }
        }
    }  
};

class Foo => {
    is => [ 'FooBase' ],
    instance => {
        methods => {
            foo => sub { 
                my $self = shift;
                return 'SUPER -> ' . $self->SUPER::foo() 
            },
            bar => sub { 
                my $self = shift;
                return 'SUPER -> ' . $self->SUPER::bar('test') 
            }            
        }
    }  
};

my $foo = Foo->new();
isa_ok($foo, 'Foo');

is($foo->foo(), 'SUPER -> FooBase::foo', '... got ther wrapped SUPER call');
is($foo->bar(), 'SUPER -> FooBase::bar->(test)', '... got ther wrapped SUPER call w/ arguments');
