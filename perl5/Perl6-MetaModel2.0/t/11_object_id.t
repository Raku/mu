#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaModel;

my $Foo = class 'Foo' => { is => [ $::Object ] };

# test all the same class
my $total = 50;
is(scalar keys %{{ map { $_->id => undef } map { $Foo->new() } 1 .. $total }}, $total, '... got all unique ids');

my $Bar   = class Bar   => { is => [ $::Object ] };
my $Baz   = class Baz   => { is => [ $::Object ] };
my $Bling = class Bling => { is => [ $::Object ] };
my $Blam  = class Blam  => { is => [ $::Object ] };
my $Bow   = class Bow   => { is => [ $::Object ] };

my @classes = ($Foo, $Bar, $Baz, $Bling, $Blam, $Bow);

is(scalar keys %{{ 
        map { 
            $_->id => undef 
        } map { 
            ($classes[(rand() * 100) % scalar @classes])->new() 
        } 1 .. $total 
    }}, $total, '... got all unique ids across different classes');
