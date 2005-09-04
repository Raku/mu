#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;

use Perl6::MetaModel;

my $Foo = class 'Foo' => {
    is => [ $::Object ],    
    methods => {
        foo => sub { 'Foo::foo' }
    }
};

my $Bar = class 'Bar' => {
    is => [ $Foo ],
    methods => {
        foo => sub { 
            'Bar::foo -> ' . next_METHOD;
        }
    }
};

my $bar = $Bar->new();
isa_ok($bar, 'Bar');

is($bar->foo(), 'Bar::foo -> Foo::foo', '... got the value expected after next METHOD call');

my $Baz = class 'Baz' => {
    is => [ $Bar ],
    methods => {
        foo => sub { 
            'Baz::foo -> ' . next_METHOD;
        }
    }
};

my $baz = $Baz->new();
isa_ok($baz, 'Baz');

is($baz->foo(), 'Baz::foo -> Bar::foo -> Foo::foo', '... got the value expected after next METHOD call');
