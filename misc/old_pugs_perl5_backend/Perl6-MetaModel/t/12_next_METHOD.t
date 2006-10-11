#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;

use Perl6::MetaModel;

my $Foo = class 'Foo' => {
    is => [ $::Object ],    
    methods => {
        foo => sub { 
            shift;
            'Foo::foo(' . (join ", " => @_) . ')' 
        }
    }
};

my $Bar = class 'Bar' => {
    is => [ $Foo ],
    methods => {
        foo => sub { 
            shift;
            'Bar::foo(' . (join ", " => @_) . ') -> ' . next_METHOD;
        }
    }
};

my $bar = $Bar->new();
isa_ok($bar, 'Bar');

is($bar->foo(1, 2, 3), 'Bar::foo(1, 2, 3) -> Foo::foo(1, 2, 3)', 
   '... got the value expected after next METHOD call');

my $Baz = class 'Baz' => {
    is => [ $Bar ],
    methods => {
        foo => sub { 
            shift;
            'Baz::foo(' . (join ", " => @_) . ') -> ' . next_METHOD;
        }
    }
};

my $baz = $Baz->new();
isa_ok($baz, 'Baz');

is($baz->foo(4, 5, 6), 'Baz::foo(4, 5, 6) -> Bar::foo(4, 5, 6) -> Foo::foo(4, 5, 6)', 
   '... got the value expected after next METHOD call');
