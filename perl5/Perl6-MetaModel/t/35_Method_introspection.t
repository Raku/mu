#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 7;

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Method;

=pod

This test file checks method introspection.

NOTE: (Stevan)
I actually think that getmethod() API as described in A12 has 2 problems:

1) It should really be get_methods instead, which 
   conforms more to accepted perl-style.

2) I think this should actually return a proxy object, meant for informational
   reflection. This would allow more information to be returned that is needed
   to be held in the actual Method object. And it would be lazy, so the info
   is only gathered when it is needed, and no sooner.
   
However, all this said, I have stubbed the methods in Perl6::Method anyway.

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
my $cat_class = ::meta($cat);
ok(::dispatch($cat_class, 'getmethods'), '... $cat_class->can(getmethods)');

my @methods   = ::dispatch($cat_class, 'getmethods');
is( @methods, 3, 'getmethods() should return descriptor for each method' );

for my $introspector (qw( name signature returns multi do ))
{
    can_ok( $methods[0], $introspector );
}
