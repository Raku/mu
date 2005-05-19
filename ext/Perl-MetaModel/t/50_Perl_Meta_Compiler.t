#!/usr/bin/pugs

use v6;
use Test;

plan 13;

use Perl::Meta::Compiler;

use Perl::Meta::Class;
use Perl::Meta::Property;
use Perl::Meta::Type;
use Perl::Meta::Method;

## Perl::Package
my $meta_package = Perl::Meta::Class::new('Perl::Package');

# has %.variables;
$meta_package.addProperty('variables',
    Perl::Meta::Property.new(type => MkType('Hash'))
);

# has %.subs;
$meta_package.addProperty('subs', 
    Perl::Meta::Property.new(type => MkType('Hash'))
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
    $package = ::Perl::Package.new();
}, '... we can create a Perl::Package instance';
ok($package.isa('Perl::Package'), '... the $package instance is a Perl::Package');
ok($package.meta() =:= $meta_package, '... the $package.meta() is $meta_package');

my $role;
lives_ok {
    $role = ::Perl::Role.new();
}, '... we can create a Perl::Role instance';
ok($role.isa('Perl::Role'), '... the $role instance is a Perl::Role');
ok($role.isa('Perl::Package'), '... the $role instance is a Perl::Package');
ok($role.meta() =:= $meta_role, '... the $role.meta() is $meta_role');

my $class;
lives_ok {
    $class = ::Perl::Class.new();
}, '... we can create a Perl::Class instance';
ok($class.isa('Perl::Class'), '... the $class instance is a Perl::Class');
ok($class.isa('Perl::Role'), '... the $class instance is a Perl::Role');
ok($class.isa('Perl::Package'), '... the $class instance is a Perl::Package');
ok($class.meta() =:= $meta_class, '... the $class.meta() is $meta_class');
