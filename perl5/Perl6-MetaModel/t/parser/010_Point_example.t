#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::MetaModel::Parser;

my $source = q|

class Point {
    has $.x;
    has $.y;
    
    method x {
        shift;
        die "Cannot assign to a read-only accessor" if @_;
        $.x;
    }
    
    method y {
        shift;
        $.y = shift if @_;
        $.y;
    }
    
    method clear {
        $.x = 0;
        $.y = 0;    
    }    
}

class Point3D is Point {
    has $:z;

    method get_z { $:z }
    
    method clear {
        $:z = 0;
        next METHOD;
    }
}


|;

my $p = Perl6::MetaModel::Parser->new();
isa_ok($p, 'Perl6::MetaModel::Parser');

$p->parse($source);

my $Point = $::{'*'}->FETCH('Point');
isa_ok($Point, 'Class');
isa_ok($Point, 'Point');

my $Point3D = $::{'*'}->FETCH('Point3D');
isa_ok($Point3D, 'Class');
isa_ok($Point3D, 'Point');
isa_ok($Point3D, 'Point3D');

## Point tests

my $point = $Point->new('$.x' => 1, '$.y' => 3);
isa_ok($point, 'Point');

is($point->y, 3, '... the y attribute is accessible and assigned to correctly in the constructor');

lives_ok { $point->y(42) } '... assigned to y attribute successfully';
is($point->y, 42, '... the y attribute was assigned to correctly');

is($point->x, 1, '... the y attribute is accessible and assigned to correctly in the constructor');

dies_ok { $point->x(-1) } '... read-only accessors die when you try to assign to them';

lives_ok { $point->clear() } '... called clear successfully';

is($point->x, 0, '... clear() set the x attribute correctly');
is($point->y, 0, '... clear() set the y attribute correctly');

## Point3D tests

my $point3D = $Point3D->new('$.x' => 2, '$.y' => 3, '$:z' => 4);
isa_ok($point3D, 'Point3D');
isa_ok($point3D, 'Point');

dies_ok { $point3D->z } '... no accessors created for private attributes';

is($point3D->x, 2, '... new() set the x attribute correctly');
is($point3D->y, 3, '... new() set the y attribute correctly');
is($point3D->get_z, 4, '... new() set the z attribute correctly');

lives_ok { $point3D->clear() } '... called clear successfully';

is($point3D->x, 0, '... clear() set the x attribute correctly');
is($point3D->y, 0, '... clear() set the y attribute correctly');
is($point3D->get_z, 0, '... clear() set the z attribute correctly');

 