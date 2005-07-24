#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;

use Perl6::MetaModel;
use Perl6::Object;

class Foo => {
    is => [ 'Perl6::Object' ],    
    instance => {
        methods => {
            foo => sub { 'Foo::foo' }
        }
    }
};

class Bar => {
    is => [ 'Foo' ],
    instance => {
        methods => {
            foo => sub { 
                'Bar::foo -> ' . next_METHOD;
            }
        }
    }
};

my $bar = Bar->new();
isa_ok($bar, 'Bar');

is($bar->foo(), 'Bar::foo -> Foo::foo', '... got the value expected after next METHOD call');

class Baz => {
    is => [ 'Bar' ],
    instance => {
        methods => {
            foo => sub { 
                'Baz::foo -> ' . next_METHOD;
            }
        }
    }
};

my $baz = Baz->new();
isa_ok($baz, 'Baz');

is($baz->foo(), 'Baz::foo -> Bar::foo -> Foo::foo', '... got the value expected after next METHOD call');
