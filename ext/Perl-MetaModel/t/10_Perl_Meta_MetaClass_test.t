#!/usr/bin/pugs

use v6;
use Test;

use Perl::Meta::MetaClass;

my $role = Perl::Meta::MetaClass::new('Role');

is($role.name(), 'Role', '... we got the right class name');
ok($role.isA($role), '... $role is-a $role');

# Super Class

my $package =  Perl::Meta::MetaClass::new('Package');

is($role.superclass(), undef, '... we do not have a superclass');
ok($package.isA($package), '... $package is-a Package');

$role.superclass($package);
is($role.superclass().name(), 'Package', '... we now have a superclass');

ok($role.isA($package), '... $role is-a $package');

{
    my @superclasses = $role.allSuperclasses();
    is(+@superclasses, 1, '... $role has one superclass');
    ok(@superclasses[0] =:= $package, '... and it is $package');
}

# confirm the circular reference ...

{
    my @subclasses = $package.subclasses();
    is(+@subclasses, 1, '... $package has one subclass');
    ok(@subclasses[0] =:= $role, '... and it is the $role');
}

# make sure we *dont* do circular inheritence

dies_ok {
    $package.superclass($role);
}, '... cannot make a subclass into a superclass';
like($!, rx:perl5/^The super class cannot inherit from the invocant \(circular inheritance\)/, '... got the right error');

# Sub Classes

{
    my @subclasses = $role.subclasses();
    is(+@subclasses, 0, '... no subclasses yet');
}

my $class =  Perl::Meta::MetaClass::new('Class');
$class.superclass($role);

{
    my @superclasses = $class.allSuperclasses();
    is(+@superclasses, 2, '... $class has two superclasses');
    ok(@superclasses[0] =:= $role, '... the first one is $role');
    ok(@superclasses[1] =:= $package, '... and the second one is $package');
}

{
    my @subclasses = $role.subclasses();
    is(+@subclasses, 1, '... we have 1 subclasses now');
    ok(@subclasses[0] =:= $class, '... this is our first subclass');
}

ok($class.isA($class), '... $class is-a $class');
ok($class.isA($role), '... $class is-a $role');
ok($class.isA($package), '... $class is-a $package');

# change the superclass
$class.superclass($package);

{ # role should no longer have the $class as a subclass
    my @subclasses = $role.subclasses();
    is(+@subclasses, 0, '... no subclasses for $role anymore');
}

{ # and it should be in $package now
    my @subclasses = sort { $^a.name() cmp $^b.name() } $package.subclasses();
    is(+@subclasses, 2, '... we now have 2 subclasses in $package');
    ok(@subclasses[0] =:= $class, '... this is our first subclass');    
    ok(@subclasses[1] =:= $role, '... this is second first subclass');    
}

# change it back now
$class.superclass($role);

# now everything is back to "normal"
{
    my @subclasses = $role.subclasses();
    is(+@subclasses, 1, '... 1 subclass for $role again');
    ok(@subclasses[0] =:= $class, '... this is our first subclass');    
}

{
    my @subclasses = $package.subclasses();
    is(+@subclasses, 1, '... we now have 1 subclass in $package again');
    ok(@subclasses[0] =:= $role, '... this is our first subclass');       
}
