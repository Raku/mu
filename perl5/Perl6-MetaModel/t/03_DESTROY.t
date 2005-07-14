#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 2;
use Data::Dumper;

use Perl6::MetaModel;

my @classes_destroyed;

class Foo => {
    instance => {
        DESTROY => sub {
            push @classes_destroyed, 'Foo::DESTROY';
        }
    }
};

class Bar => {
    is => [ 'Foo' ],
    instance => {
        DESTROY => sub {
            push @classes_destroyed, 'Bar::DESTROY';
        }
    }
};

class 'Foo::Bar' => {
    is => [ 'Bar' ],
    instance => {
        DESTROY => sub {
            push @classes_destroyed, 'Foo::Bar::DESTROY';
        }
    }
};


{
    my $foobar = Foo::Bar->new();
    isa_ok($foobar, 'Foo::Bar');
}

is_deeply(
    \@classes_destroyed,
    [ 'Foo::Bar::DESTROY', 'Bar::DESTROY', 'Foo::DESTROY' ],
    '... classes were destroyed in the right order');
