#!/usr/bin/pugs

use v6;
use Test;

use Perl::Meta::MetaClass;

# --------------------------------------------------------------
# This test attempts to model the following class.
# --------------------------------------------------------------
#
#         Package        <- Super-MetaClass
#            |
#          Role          <- MetaClass
#            |
#    [ .prop1 .prop2 ]   <- Properties
#            |
#   [ method1 method2 ]  <- Methods
#            |
#          Class         <- SubClasses
#
# --------------------------------------------------------------

my $role = Perl::Meta::MetaClass::new('Role');

is($role.clsName(), 'Role', '... we got the right class name');
ok($role.clsIsa('Role'), '... $role is-a Role');
ok($role.clsIsa($role), '... $role is-a $role');

# Super Class

my $package =  Perl::Meta::MetaClass::new('Package');

is($role.clsSuper(), undef, '... we do not have a superclass');
ok($package.clsIsa('Package'), '... $package is-a Package');

$role.clsSuper($package);
is($role.clsSuper().clsName(), 'Package', '... we now have a superclass');

ok($role.clsIsa('Package'), '... $role is-a Package');
ok($role.clsIsa($package), '... $role is-a $package');

# confirm the circular reference ...

{
    my @subclasses = $package.clsSubClasses().members();
    is(+@subclasses, 1, '... $package has one subclass');
    ok(@subclasses[0] =:= $role, '... and it is the $role');
}

# make sure we *dont* do circular inheritence

dies_ok {
    $package.clsSuper($role);
}, '... cannot make a subclass into a superclass';
like($!, rx:perl5/^The super class cannot inherit from the invocant \(circular inheritance\)/, '... got the right error');

# Sub Classes

{
    my @subclasses = $role.clsSubClasses().members();
    is(+@subclasses, 0, '... no subclasses yet');
}

my $class =  Perl::Meta::MetaClass::new('Class');
$class.clsSuper($role);

{
    my @subclasses = $role.clsSubClasses().members();
    is(+@subclasses, 1, '... we have 1 subclasses now');
    ok(@subclasses[0] =:= $class, '... this is our first subclass');
}

ok($class.clsIsa('Class'), '... $class is-a Class');
ok($class.clsIsa($class), '... $class is-a $class');

ok($class.clsIsa('Role'), '... $class is-a Role');
ok($class.clsIsa($role), '... $class is-a $role');

ok($class.clsIsa('Package'), '... $class is-a Package');
ok($class.clsIsa($package), '... $class is-a $package');

ok(!$class.clsIsa('Nothing'), '... $class is-not-a Nothing');
ok(!$role.clsIsa('Nothing'), '... $role is-not-a Nothing');
ok(!$package.clsIsa('Nothing'), '... $package is-not-a Nothing');

# change the superclass
$class.clsSuper($package);

{ # role should no longer have the $class as a subclass
    my @subclasses = $role.clsSubClasses().members();
    is(+@subclasses, 0, '... no subclasses for $role anymore');
}

{ # and it should be in $package now
    my @subclasses = sort { $^a.clsName() cmp $^b.clsName() } $package.clsSubClasses().members();
    is(+@subclasses, 2, '... we now have 2 subclasses in $package');
    ok(@subclasses[0] =:= $class, '... this is our first subclass');    
    ok(@subclasses[1] =:= $role, '... this is second first subclass');    
}

# change it back now
$class.clsSuper($role);

# now everything is back to "normal"
{
    my @subclasses = $role.clsSubClasses().members();
    is(+@subclasses, 1, '... 1 subclass for $role again');
    ok(@subclasses[0] =:= $class, '... this is our first subclass');    
}

{
    my @subclasses = $package.clsSubClasses().members();
    is(+@subclasses, 1, '... we now have 1 subclass in $package again');
    ok(@subclasses[0] =:= $role, '... this is our first subclass');       
}

dies_ok {
    $class.clsSubClasses($role);
}, '... subclass cannot be the superclass of the invocant';
like($!, rx:perl5/^Sub class\'s superclass must be the invocant/, '... got the right error');
