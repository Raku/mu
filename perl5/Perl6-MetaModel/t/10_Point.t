#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 19;
use Test::Exception; 

require Perl6::MetaModel::Genesis;

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

my $Point = $::Class->new('$:name' => 'Point');

$Point->superclasses([ $::Object ]);

$Point->add_attribute('$.x' => ::make_attribute('$.x'));
$Point->add_attribute('$.y' => ::make_attribute('$.y'));

$Point->add_method('x' => ::make_method(sub {
    my $self = shift;
    die "Cannot assign to a read-only accessor" if @_;
    ::opaque_instance_attr($self => '$.x');
}));

$Point->add_method('y' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attr($self => '$.y') = shift if @_;
    ::opaque_instance_attr($self => '$.y');
}));

$Point->add_method('clear' => ::make_method(sub {
    ::opaque_instance_attr($::SELF => '$.x') = 0;
    ::opaque_instance_attr($::SELF => '$.y') = 0;    
}));

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

=pod

    class Point3d is Point {
        has $:z = 123;
        method clear () { $:z = 0; next; }
    }

    my $point3d = Point3d.new(x => 2, y => 3, z => 4);
    $c = $point3d.z;    # illegal, $:z is invisible

=cut

my $Point3D = $::Class->new('$:name' => 'Point3D');

$Point3D->superclasses([ $Point ]);

$Point3D->add_attribute('$:z' => ::make_attribute('$:z'));

$Point3D->add_method('get_z' => ::make_method(sub {
    ::opaque_instance_attr($::SELF => '$:z');
}));

$Point3D->add_method('clear' => ::make_method(sub {
    ::opaque_instance_attr($::SELF => '$:z') = 0;
    ::next_METHOD()
}));

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



{
    # avoid warnings ...
    my @void = ($::Object, $::Class);
}
