#!/usr/bin/pugs

use v6;
use Test;

plan 38;

=pod

This test demonstrates the use of the Perl::Meta::Compiler.

NOTE:
The object model created here is by no means the actual 
perl6 object model. It is just an experiment with the 
class in Perl::Meta::* and how they work with the 
Perl::Meta::Compiler itself.

=cut

use Perl::Meta::Compiler;

use Perl::Meta::MetaClass;
use Perl::Meta::Property;
use Perl::Meta::Type;
use Perl::Meta::Method;

## Perl::Package
my $meta_package = Perl::Meta::MetaClass.new('Perl::Package');

# has $.version;
$meta_package.addProperty('version',
    Perl::Meta::Property.new(type => MkType('Rat'))
);

# has %.variables;
$meta_package.addProperty('variables',
    Perl::Meta::Property.new(type => MkType('Hash'), :trait<rw>)
);

# has %.subs;
$meta_package.addProperty('subs', 
    Perl::Meta::Property.new(type => MkType('Hash'), :trait<rw>)
);

$meta_package.addMethod('name', 
    Perl::Meta::Method.new(
        code => sub ($self) { $self.meta().name() }
    )
);

$meta_package.addMethod('identifier', 
    Perl::Meta::Method.new(
        code => sub { $_.name() ~ '-' ~ $_.version() }
    )
);

## Perl::Role
my $meta_role = Perl::Meta::MetaClass.new('Perl::Role');

# isa Perl::Package;
$meta_role.superclass($meta_package);

# has @.super_roles;
$meta_role.addProperty('super_roles',
    Perl::Meta::Property.new(type => MkType('Array'), :trait<rw>)
);

# has %.properties;
$meta_role.addProperty('properties',
    Perl::Meta::Property.new(type => MkType('Hash'))
);

# has Perl::Method %.methods;
$meta_role.addProperty('methods',
    Perl::Meta::Property.new(type => MkType('Hash'))
);

$meta_role.addMethod('does', 
    Perl::Meta::Method.new(
        code => sub ($self, @roles) { 
            $self.super_roles().push(@roles) if @roles;
            $self.super_roles();         
        }
    )    
);

## Perl::Class
my $meta_class = Perl::Meta::MetaClass.new('Perl::Class');

# isa Perl::Role;
$meta_class.superclass($meta_role);

# has @.super_classes;
$meta_class.addProperty('super_classes',
    Perl::Meta::Property.new(type => MkType('Array'), :trait<rw>)
);

$meta_class.addMethod('is', 
    Perl::Meta::Method.new(
        code => sub ($self, @classes) { 
            $self.super_classes().push(@classes) if @classes;
            $self.super_classes();         
        }
    )    
);

## now make a compiler

my $compiler = Perl::Meta::Compiler.new();

lives_ok {
    $compiler.compileAll($meta_package);
}, '... we can compile our class tree';

my $package;
lives_ok {
    $package = ::Perl::Package.new(:version(0.01));
}, '... we can create a Perl::Package instance';
ok($package.isa('Perl::Package'), '... the $package instance is a Perl::Package');
ok($package.meta() =:= $meta_package, '... the $package.meta() is $meta_package');

is($package.version(), 0.01, '... got the right version number');

ok($package.can('name'), '... we $package.can() call the name() method');
ok($package.can('identifier'), '... we $package.can() call the identifier() method');

is($package.name(), 'Perl::Package', '... got the right name');
is($package.identifier(), 'Perl::Package-0.01', '... got the right identifier');

my $role;
lives_ok {
    $role = ::Perl::Role.new(:version(0.01));
}, '... we can create a Perl::Role instance';
ok($role.isa('Perl::Role'), '... the $role instance is a Perl::Role');
ok($role.isa('Perl::Package'), '... the $role instance is a Perl::Package');
ok($role.meta() =:= $meta_role, '... the $role.meta() is $meta_role');

is($role.version(), 0.01, '... got the right version number');

ok($role.can('name'), '... we $role.can() call the name() method');
ok($role.can('identifier'), '... we $role.can() call the identifier() method');
ok($role.can('does'), '... we $role.can() call the does() method');

is($role.name(), 'Perl::Role', '... got the right name');
is($role.identifier(), 'Perl::Role-0.01', '... got the right identifier');

my @roles = (::Perl::Role.new(), ::Perl::Role.new()); # just empty roles for now
lives_ok {
    $role.does(@roles);
}, '... we added roles succussfully';

{
    my $my_roles = $role.does();
    is(+$my_roles, 2, '... we got back 2 roles');
    ok($my_roles[0] =:= @roles[0], '... this is the correct role');
    ok($my_roles[1] =:= @roles[1], '... this is the correct role');    
}

my $class;
lives_ok {
    $class = ::Perl::Class.new(:version(0.01));
}, '... we can create a Perl::Class instance';
ok($class.isa('Perl::Class'), '... the $class instance is a Perl::Class');
ok($class.isa('Perl::Role'), '... the $class instance is a Perl::Role');
ok($class.isa('Perl::Package'), '... the $class instance is a Perl::Package');
ok($class.meta() =:= $meta_class, '... the $class.meta() is $meta_class');

is($class.version(), 0.01, '... got the right version number');

ok($class.can('name'), '... we $class.can() call the name() method');
ok($class.can('identifier'), '... we $class.can() call the identifier() method');
ok($class.can('is'), '... we $class.can() call the is() method');

is($class.name(), 'Perl::Class', '... got the right name');
is($class.identifier(), 'Perl::Class-0.01', '... got the right identifier');

my @classes = (::Perl::Class.new(), ::Perl::Class.new()); # just empty classes for now
lives_ok {
    $class.is(@classes);
}, '... we added classes succussfully';

{
    my $my_classes = $class.is();
    is(+$my_classes, 2, '... we got back 2 classs');
    ok($my_classes[0] =:= @classes[0], '... this is the correct class');
    ok($my_classes[1] =:= @classes[1], '... this is the correct class');    
}
