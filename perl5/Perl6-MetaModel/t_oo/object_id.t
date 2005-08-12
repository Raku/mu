#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaModel;

=pod

this test was converted from t/oo/object_id.t

=cut

class 'Foo' => { 'is' => [ 'Perl6::Object' ] };

my $num_objects = 20;

my %foos;
foreach (1 .. $num_objects) {
    my $f = Foo->new();
    $foos{$f->id()}++;
}

is(scalar keys %foos, $num_objects, '... all our .id()s were unique');
