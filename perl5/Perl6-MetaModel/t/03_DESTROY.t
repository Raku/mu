#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;
use Data::Dumper;

=pod

Checks destruction order. 

This file also tests diamond inheritance destruction order
and assures that using the C3 ordering method, we do not
get duplicate DESTRUCTION

=cut

use Perl6::MetaModel;

{
    my @classes_destroyed;

    class Foo => {
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };

    class Bar => {
        is => [ 'Foo' ],
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };

    class 'Foo::Bar' => {
        is => [ 'Bar' ],
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };


    {
        my $foobar = Foo::Bar->new();
        isa_ok($foobar, 'Foo::Bar');
        isa_ok($foobar, 'Bar');
        isa_ok($foobar, 'Foo');                
    }

    is_deeply(
        \@classes_destroyed,
        [ 'Foo::Bar::DESTROY', 'Bar::DESTROY', 'Foo::DESTROY' ],
        '... classes were destroyed in the right order');
}    

# diamond pattern destruction (no duplicate calls)

{
    my @classes_destroyed;

    class A => {
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };

    class B => {
        is => [ 'A' ],
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };
    
    class C => {
        is => [ 'A' ],
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };    

    class D => {
        is => [ 'B', 'C' ],
        instance => {
            DESTROY => sub {
                push @classes_destroyed, (CLASS . '::DESTROY');
            }
        }
    };


    {
        my $d = D->new();
        isa_ok($d, 'D');
        isa_ok($d, 'C');        
        isa_ok($d, 'B');        
        isa_ok($d, 'A');        
    }

    is_deeply(
        \@classes_destroyed,
        [ 'D::DESTROY', 'B::DESTROY', 'C::DESTROY', 'A::DESTROY' ],
        '... classes were destroyed in the right order');
}

