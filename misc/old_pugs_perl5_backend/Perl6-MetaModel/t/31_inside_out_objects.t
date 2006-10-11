#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 15;
use Test::Exception;

use Perl6::MetaModel;

=pod

This is a minimal proof of contept that the perl 6 metamodel
can host an inside-out-objects based class model. 

=cut

my $Foo = class 'Foo' => {
    is => [ $::Object ],
    class_attributes => [
        '%:bars'
    ],
    class_methods => {
        'instance_count' => sub { scalar keys %{__('%:bars')} }
    },
    methods => {
        'bar' => sub {
            shift;
            __('%:bars')->{$::SELF->id} = shift if @_;
            __('%:bars')->{$::SELF->id};
        }
    },
    submethods => {
        'BUILD' => sub {
            my ($self, %params) = @_;
            __('%:bars')->{$self->id} = $params{'$.bar'} || undef;
        },
        'DESTROY' => sub {
            delete __('%:bars')->{$::SELF->id};
        }
    }
};
isa_ok($Foo, 'Class');
isa_ok($Foo, 'Foo');

is($Foo->instance_count, 0, '... no instances to count');

{   
    my $iFoo = $Foo->new();
    isa_ok($iFoo, 'Foo');
    
    is($Foo->instance_count, 1, '... one instances to count');    

    ok(!defined($iFoo->bar), '... no instance variable in first instance');

    $iFoo->bar(5);
    is($iFoo->bar, 5, '... got the instance value in first instance');

    {
        my $iFoo2 = $Foo->new();
        isa_ok($iFoo2, 'Foo');
        
        is($Foo->instance_count, 2, '... two instances to count');            

        ok(!defined($iFoo2->bar), '... no instance value in second instance');
        is($iFoo->bar, 5, '... got the (unchanged) instance value in first instance');
        
        $iFoo2->bar(15);
        is($iFoo2->bar, 15, '... got the instance value in second instance');
        is($iFoo->bar, 5, '... got the (unchanged) instance value in first instance');
    }
    
    is($Foo->instance_count, 1, '... one instances to count');        
}

is($Foo->instance_count, 0, '... no instances to count');