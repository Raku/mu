#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 2;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

class Foo => { is => [ 'Perl6::Object' ] };

# test all the same class
my $total = 50;
is(scalar keys %{{ map { $_->id => undef } map { Foo->new() } 1 .. $total }}, $total, '... got all unique ids');

class Bar   => { is => [ 'Perl6::Object' ] };
class Baz   => { is => [ 'Perl6::Object' ] };
class Bling => { is => [ 'Perl6::Object' ] };
class Blam  => { is => [ 'Perl6::Object' ] };
class Bow   => { is => [ 'Perl6::Object' ] };

my @classes = qw(Foo Bar Baz Bling Blam Bow);

is(scalar keys %{{ 
        map { 
            $_->id => undef 
        } map { 
            ($classes[(rand() * 100) % scalar @classes])->new() 
        } 1 .. $total 
    }}, $total, '... got all unique ids across different classes');
