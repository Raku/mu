#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;
use Test::Exception;

use Perl6::MetaModel;

=pod

Testing the ability to disambiguate a method call
by using the $::Package->FETCH('&method') syntax

=cut

my $Foo = class 'Foo' => {
    is => [ $::Object ],
    methods => {
        'baz' => sub { 'Foo::baz' }
    }
};

my $Bar = class 'Bar' => {
    is => [ $::Object ],
    methods => {
        'baz' => sub { 'Bar::baz' }
    }
};

my $FooBar = class 'Foo::Bar' => {
    is => [ $Foo, $Bar ],
    methods => {
        'Bar_baz' => sub { 
            my $method = $Bar->FETCH('&baz');
            $::SELF->$method();
        }
    }
};

my $foobar = $FooBar->new();
isa_ok($foobar, 'Foo::Bar');

is($foobar->baz(), 'Foo::baz', '... go the expected value from normal method call');
is($foobar->Bar_baz(), 'Bar::baz', '... got the expected value from the disambiguated method call');
