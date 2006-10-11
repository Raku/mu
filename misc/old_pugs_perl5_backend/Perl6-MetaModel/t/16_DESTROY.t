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

    # DESTROY as a method
    my $Foo = class 'Foo' => {
        'is' => [ $::Object ],
        'methods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('Foo::DESTROY');
            }
        }
    };

    # DESTROY as a submethod
    my $Bar = class 'Bar' => {
        'is' => [ $Foo ],
        'submethods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('Bar::DESTROY');
            }
        }
    };

    my $FooBar = class 'Foo::Bar' => {
        'is' => [ $Bar ],
        'methods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('Foo::Bar::DESTROY');
            }
        }
    };


    {
        my $foobar = $FooBar->new();
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

    my $A = class 'A' => {
        'is' => [ $::Object ],
        'methods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('A::DESTROY');
            }
        }
    };

    my $B = class 'B' => {
        'is' => [ $A ],
        'methods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('B::DESTROY');
            }
        }
    };

    my $C = class 'C' => {
        'is' => [ $A ],
        'methods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('C::DESTROY');
            }
        }
    };    

    my $D = class 'D' => {
        'is' => [ $B, $C ],
        'methods' => {
            'DESTROY' => sub {
                push @classes_destroyed, ('D::DESTROY');
            }
        }
    };


    {
        my $d = $D->new();
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

