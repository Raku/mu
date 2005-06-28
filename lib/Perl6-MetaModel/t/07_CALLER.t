#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::MetaModel;

=pod

=cut

class Foo => {
    class => {
        methods => {        
            bar => sub {
                my $self = shift;
                $self->baz();
            },
            baz => sub { 'hello ' . (join "::" => CALLER(1)) . ' from baz' }
        }
    }
};


my $foo = Foo->new_instance();
isa_ok($foo, 'Foo');

is($foo->bar(), 'hello Foo::bar from baz', '... got the right output related to CALLER');

class Foo2 => {
    extends => 'Foo',
    class => {
        methods => {        
            bar => sub {
                my $self = shift;
                $self->SUPER('bar');
            },
        }
    }
};

my $foo2 = Foo2->new_instance();
isa_ok($foo2, 'Foo2');

is($foo2->bar(), 'hello Foo2::SUPER::bar from baz', '... got the right output related to CALLER');
