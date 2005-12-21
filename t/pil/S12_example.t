#!/usr/bin/pugs

use v6;
use Test::PIL::Bootstrap;

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

my $point = q:to/POINT/
::Point := ::Class.new({});
::Point.set_name("Point");
::Point.set_superclasses([ ::Object ]);

::Point.add_attribute('$x', 0);
::Point.add_attribute('$y', 0);

::Point.add_method('x',     ->    { self.get_attr('$x')     });
::Point.add_method('y',     ->    { self.get_attr('$y')     });
::Point.add_method('set_y', -> $y { self.set_attr('$y', $y) });
::Point.add_method('clear', -> { 
    self.set_attr('$x', 0);
    self.set_attr('$y', 0);     
});

$point := ::Point.new({ '$x' => 1, '$y' => 3 });
POINT;

pil_is_eq($point ~ '$point.isa("Point")', 'true', '... $point.isa(Point)');

pil_is_eq($point ~ '$point.x()', '1', '... $point.x() == 1');
pil_is_eq($point ~ '$point.y()', '3', '... $point.y() == 3');

pil_is_eq($point ~ '$point.set_y(42); $point.y()', '42', '... $point.y() == 42 now');

pil_is_eq($point ~ q:to/CODE/
$point.clear();
[ $point.x(), $point.y() ];
CODE, 
'[0, 0]', 
'... $point.clear() && $point.x == 0, $point.y == 0');

=pod

    class Point3d is Point {
        has $:z = 123;
        method clear () { $:z = 0; next; }
    }

    my $point3d = Point3d.new(x => 2, y => 3, z => 4);
    $c = $point3d.z;    # illegal, $:z is invisible

=cut

my $point3d = $point ~ q:to/POINT3D/
::Point3D := ::Class.new({});
::Point3D.set_name("Point3D");
::Point3D.set_superclasses([ ::Point ]);

::Point3D.add_attribute('$!z', 0);

::Point3D.add_method('get_z',     -> { self.get_attr('$!z') });
::Point3D.add_method('clear', -> { 
    self.set_attr('$!z', 0);
    &?NEXT.();
});

$point3d := ::Point3D.new({ '$x' => 2, '$y' => 3, '$!z' => 4 });
POINT3D;

pil_is_eq($point3d ~ '$point3d.isa("Point")', 'true', '... $point3d.isa(Point)');
pil_is_eq($point3d ~ '$point3d.isa("Point3D")', 'true', '... $point3d.isa(Point3D)');

pil_is_eq($point3d ~ '$point3d.x()', '2', '... $point3d.x() == 2');
pil_is_eq($point3d ~ '$point3d.y()', '3', '... $point3d.y() == 3');
pil_is_eq($point3d ~ '$point3d.get_z()', '4', '... $point3d.get_z() == 4');

pil_is_eq($point3d ~ q:to/CODE/
$point3d.clear();
[ $point3d.x(), $point3d.y(), $point3d.get_z() ]
CODE, 
'[0, 0, 0]', 
'... $point3d.clear() && $point3d.x == 0, $point3d.y == 0, $point3d.z == 0');