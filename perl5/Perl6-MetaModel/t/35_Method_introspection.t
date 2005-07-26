#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 7;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Method;

=pod

This test file checks method introspection.

=cut

class 'Cat-0.0.1-cpan:CHROMATIC' =>
{
    is    => [ 'Perl6::Object' ],
    class =>
    {
        methods =>
        {
            name  => sub { __('$.name')  || 'anonymous cat'   },
            color => sub { __('$.color') || 'ambiguous brown' },
        },
    },
    instance => 
    {
        attrs => [ '$.name', '$.color' ],
        methods =>
        {
            speak => sub { 'meow' },
        },
    },
};

my $cat       = Cat->new( '$.name' => 'Fluffy', '$.color' => 'white' );
my $cat_class = $cat->meta();
can_ok( $cat_class, 'getmethods' );

my @methods   = $cat_class->getmethods();
is( @methods, 3, 'getmethods() should return descriptor for each method' );

for my $introspector (qw( name signature returns multi do ))
{
    can_ok( $methods[0], $introspector );
}
