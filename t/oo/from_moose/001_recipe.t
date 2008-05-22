use v6;

use Test;

class Point {
    has $.x is ro;
    has $.y is rw;

    method clear () {
        $.x = $.y = 0;
    }
#    $?CLASS.^make_immutable(debug => 0);
}

class Point3D is Point {
    has Int $z;

    method get_z { $z; }

    method clear () {
        self.SUPER::clear;
        $z = 0;
    }
#    $?CLASS.^make_immutable(debug => 0);
}

# In Moose, Point3D uses after to implement the clear method, I still don't
# have an idea on how this looks like in perl 6.
#	after 'clear' => sub {
#	    my $self = shift;
#	    $self->{z} = 0;
#	};
	
my $point = Point.new(x => 1, y => 2);
isa_ok($point, Point);

is($point.x, 1, '... got the right value for x');
is($point.y, 2, '... got the right value for y');

$point.y(10);
is($point.y(), 10, '... got the right (changed) value for y');

dies_ok {
	$point.y('Foo');
}, '... cannot assign a non-Int to y';

dies_ok {
    $point.x(1000);
}, '... cannot assign to a read-only method';
is($point.x, 1, '... got the right (un-changed) value for x');

$point.clear();

is($point.x, 0, '... got the right (cleared) value for x');
is($point.y, 0, '... got the right (cleared) value for y');

# check the type constraints on the constructor

lives_ok {
	Point.new(x => 0, y => 0);
}, '... can assign a 0 to x and y';

dies_ok {
	Point.new(x => 10, y => 'Foo');
}, '... cannot assign a non-Int to y';

dies_ok {
	Point.new(x => 'Foo', y => 10);
}, '... cannot assign a non-Int to x';

# Point3D

my $point3d = Point3D.new({ x => 10, y => 15, z => 3 });
isa_ok($point3d, 'Point3D');
isa_ok($point3d, 'Point');

is($point3d.x, 10, '... got the right value for x');
is($point3d.y, 15, '... got the right value for y');
is($point3d.get_z, 3, '... got the right value for z');

dies_ok {
	$point3d.z;
}, '... there is no method for z';

$point3d.clear();

is($point3d.x, 0, '... got the right (cleared) value for x');
is($point3d.y, 0, '... got the right (cleared) value for y');
is($point3d.get_z, 0, '... got the right (cleared) value for z');

dies_ok {
	Point3D.new(x => 10, y => 'Foo', z => 3);
}, '... cannot assign a non-Int to y';

dies_ok {
	Point3D.new(x => 'Foo', y => 10, z => 3);
}, '... cannot assign a non-Int to x';

dies_ok {
	Point3D.new(x => 0, y => 10, z => 'Bar');
}, '... cannot assign a non-Int to z';

# test some class introspection

can_ok('Point', 'meta');
#isa_ok(Point->meta, 'Moose::Meta::Class');

can_ok('Point3D', 'meta');
#isa_ok(Point3D->meta, 'Moose::Meta::Class');

isnt(Point.meta, Point3D.meta, '... they are different metaclasses as well');

# poke at Point
# TODO: change below code into Perl 6 version.
# is_deeply(
# 	[ Point->meta->superclasses ],
# 	[ 'Moose::Object' ],
# 	'... Point got the automagic base class');

my @Point_methods = qw(meta new x y clear DESTROY);
my @Point_attrs   = ('x', 'y');

# TODO: change below into perl 6 version.
# is_deeply(
# 	[ sort @Point_methods                 ],
# 	[ sort Point.^get_methods() ],
# 	'... we match the method list for Point');
	
# TODO: change below into perl 6 version.
# is_deeply(
# 	[ sort @Point_attrs                      ],
# 	[ sort Point->meta->get_attribute_list() ],
# 	'... we match the attribute list for Point');	

# TODO: change below into perl 6 version.
# foreach @Point_methods -> $method {
# 	ok(Point.^has_method($method), '... Point has the method "' ~ $method ~ '"');
# }

# TODO: change below into perl 6 version.
# foreach @Point_attrs -> $attr_name {
# 	ok(Point^.has_attribute($attr_name), '... Point has the attribute "' ~ $attr_name ~ '"');    
#     my $attr = Point.^get_attribute($attr_name);
# 	ok($attr.has_type_constraint, '... Attribute ' ~ $attr_name ~ ' has a type constraint');
#	isa_ok($attr.type_constraint, 'Moose::Meta::TypeConstraint');	
#    is($attr.type_constraint.name, 'Int', '... Attribute ' ~ $attr_name ~ ' has an Int type constraint');	
#}

# poke at Point3D

# is_deeply(
# 	[ Point3D.^superclasses ],
# 	[ 'Point' ],
# 	'... Point3D gets the parent given to it');

my @Point3D_methods = qw(new meta clear DESTROY);
my @Point3D_attrs   = ('z');

# is_deeply(
# 	[ sort @Point3D_methods                 ],
# 	[ sort Point3D.^get_method_list() ],
# 	'... we match the method list for Point3D');
	
# is_deeply(
# 	[ sort @Point3D_attrs                      ],
# 	[ sort Point3D.^get_attribute_list() ],
# 	'... we match the attribute list for Point3D');	
 
# foreach @Point3D_methods -> $method {
# 	ok(Point3D.^has_method($method), '... Point3D has the method "' ~ $method ~ '"');
# }
# 
# foreach @Point3D_attrs -> $attr_name {
# 	ok(Point3D.^has_attribute($attr_name), '... Point3D has the attribute "' ~ $attr_name ~ '"');    
#     my $attr = Point3D->meta->get_attribute($attr_name);
# 	ok($attr.has_type_constraint, '... Attribute ' ~ $attr_name ~ ' has a type constraint');
# #	isa_ok($attr.type_constraint, 'Moose::Meta::TypeConstraint');	
#     is($attr.type_constraint.name, 'Int', '... Attribute ' ~ $attr_name ~ ' has an Int type constraint');	
# }
