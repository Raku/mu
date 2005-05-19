#!/usr/bin/pugs

use v6;
use Test;

plan 28;

use Perl::Meta::Compiler;

use Perl::Meta::Class;
use Perl::Meta::Property;
use Perl::Meta::Type;
use Perl::Meta::Method;

## Perl::Package
my $meta_package = Perl::Meta::Class::new('Perl::Package');

# has $.version;
$meta_package.addProperty('version',
    Perl::Meta::Property.new(type => MkType('Rat'))
);

# has %.variables;
$meta_package.addProperty('variables',
    Perl::Meta::Property.new(type => MkType('Hash'))
);

# has %.subs;
$meta_package.addProperty('subs', 
    Perl::Meta::Property.new(type => MkType('Hash'))
);

$meta_package.addMethod('name', 
    Perl::Meta::Method.new(code => sub ($self) { $self.meta().name() })
);

$meta_package.addMethod('identifier', 
    Perl::Meta::Method.new(code => sub ($self) {
       $self.name() ~ '-' ~ $self.version();
    })
);

## Perl::Role
my $meta_role = Perl::Meta::Class::new('Perl::Role');

# isa Perl::Package;
$meta_role.superclass($meta_package);

# has @.super_roles;
$meta_role.addProperty('super_roles',
    Perl::Meta::Property.new(type => MkType('Array'))
);

# has %.properties;
$meta_role.addProperty('properties',
    Perl::Meta::Property.new(type => MkType('Hash'))
);

# has Perl::Method %.methods;
$meta_role.addProperty('methods',
    Perl::Meta::Property.new(type => MkType('Hash'))
);

## Perl::Class
my $meta_class = Perl::Meta::Class::new('Perl::Class');

# isa Perl::Role;
$meta_class.superclass($meta_role);

# has @.super_classes;
$meta_class.addProperty('super_classes',
    Perl::Meta::Property.new(type => MkType('Array'))
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

is($role.name(), 'Perl::Role', '... got the right name');
is($role.identifier(), 'Perl::Role-0.01', '... got the right identifier');

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

is($class.name(), 'Perl::Class', '... got the right name');
is($class.identifier(), 'Perl::Class-0.01', '... got the right identifier');
