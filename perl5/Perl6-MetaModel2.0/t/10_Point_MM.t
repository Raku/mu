#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 19;
use Test::Exception; 

use Perl6::MetaModel;

=pod

The purpose of this test is to show how much of the default model 
described in A12 that we can currently support. The A12 examples are
above the Perl6::MetaModel equivalents

    class Point {
        has $.x;
        has $.y is rw;

        method clear () { $.x = 0; $.y = 0; }
    }

    my $point = Point.new(x => 2, y => 3);

    $a = $point.y;      # okay
    $point.y = 42;      # okay

    $b = $point.x;      # okay
    $point.x = -1;      # illegal, default is read-only

    $point.clear;       # reset to 0,0

=cut

my $Point = class 'Point' => {
    is => [ $::Object ],
    attributes => [ '$.x', '$.y' ],
    methods => {
        'x' => sub {
            shift;
            die "Cannot assign to a read-only accessor" if @_;
            _('$.x');
        },
        'y' => sub {
            shift;
            _('$.y' => shift) if @_;
            _('$.y');
        },
        'clear' => sub {
            _('$.x' => 0);
            _('$.y' => 0);    
        }
    }
};

my $point = $Point->class::new('$.x' => 1, '$.y' => 3);
isa_ok($point, 'Point');

is($point->y, 3, '... the y attribute is accessible and assigned to correctly in the constructor');

lives_ok { $point->y(42) } '... assigned to y attribute successfully';
is($point->y, 42, '... the y attribute was assigned to correctly');

is($point->x, 1, '... the y attribute is accessible and assigned to correctly in the constructor');

dies_ok { $point->x(-1) } '... read-only accessors die when you try to assign to them';

lives_ok { $point->clear() } '... called clear successfully';

is($point->x, 0, '... clear() set the x attribute correctly');
is($point->y, 0, '... clear() set the y attribute correctly');

=pod

    class Point3d is Point {
        has $:z = 123;
        method clear () { $:z = 0; next; }
    }

    my $point3d = Point3d.new(x => 2, y => 3, z => 4);
    $c = $point3d.z;    # illegal, $:z is invisible

=cut

my $Point3D = class 'Point3D' => {
    is => [ $Point ],
    attributes => [ '$:z' ],
    methods => {
        'get_z' => sub { _('$:z') },
        'clear' => sub {
            _('$:z' => 0);
            ::next_METHOD()
        }
    }
};


my $point3D = $Point3D->class::new('$.x' => 2, '$.y' => 3, '$:z' => 4);
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



{
    # avoid warnings ...
    my @void = ($::Object, $::Class);
}
