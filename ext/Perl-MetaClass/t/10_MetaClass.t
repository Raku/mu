#!/usr/bin/pugs

use v6;
use Test;

use_ok('Perl::MetaClass');
use_ok('Perl::MetaProperty');

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

my $role = Perl::MetaClass::new('Role');

is($role.clsName(), 'Role', '... we got the right class name');

# Super Class

my $package = Perl::MetaClass::new('Package');

is($role.clsSuper(), undef, '... we do not have a superclass');

$role.clsSuper($package);
is($role.clsSuper().clsName(), 'Package', '... we now have a superclass');

# confirm the circular reference ...

my @subclasses = $package.clsSubClasses();
is(+@subclasses, 1, '... $package has one subclass');
is(@subclasses[0], $role, '... and it is the $role');

# Sub Classes

{
    my @subclasses = $role.clsSubClasses();
    is(+@subclasses, 0, '... no subclasses yet');
}

my $class = Perl::MetaClass::new('Class');
$class.clsSuper($role);

{
    my @subclasses = $role.clsSubClasses();
    is(+@subclasses, 1, '... we have 1 subclasses now');
    is(@subclasses[0].clsName(), 'Class', '... this is our first subclass');
}

# change the superclass
$class.clsSuper($package);

{ # role should no longer have the $class as a subclass
    my @subclasses = $role.clsSubClasses();
    is(+@subclasses, 0, '... no subclasses for $role anymore');
}

{ # and it should be in $package now
    my @subclasses = sort { $^a.clsName() cmp $^b.clsName() } $package.clsSubClasses();
    is(+@subclasses, 2, '... we now have 2 subclasses in $package');
    is(@subclasses[0].clsName(), 'Class', '... $class is a subclass');        
    is(@subclasses[1].clsName(), 'Role', '... as is $role');
}

# change it back now
$class.clsSuper($role);

# now everything is back to "normal"
{
    my @subclasses = $role.clsSubClasses();
    is(+@subclasses, 1, '... 1 subclass for $role again');
    is(@subclasses[0].clsName(), 'Class', '... $class is a subclass of $role again');        
}

{
    my @subclasses = sort $package.clsSubClasses();
    is(+@subclasses, 1, '... we now have 1 subclass in $package again');
    is(@subclasses[0].clsName(), 'Role', '... and it is $role');
}

dies_ok {
    $class.clsSubClasses($role);
}, '... subclass cannot be the superclass of the invocant';
like($!, rx:perl5/^Sub class\'s superclass must be the invocant/, '... got the right error');

# Properties

{
    my %props = $role.clsProperties();
    is(+keys(%props), 0, '... we have no properties yet');
}

my $prop1 = Perl::MetaProperty::new('Str');
my $prop2 = Perl::MetaProperty::new('Int');

{
    my %props = $role.clsProperties('.prop1', $prop1, '.prop2', $prop2);
    my @keys = keys(%props);
    is(+@keys, 2, '... we have 2 properties now');
    is(@keys[0], '.prop1', '... we have the right property name');
    ok(%props<.prop1>.instance_isa('Perl::MetaProperty'), '... our property is a Perl::MetaProperty');
    is(%props<.prop1>.propType(), 'Str', '... the properties type is a string');

    is(@keys[1], '.prop2', '... we have the right property name');
    ok(%props<.prop2>.instance_isa('Perl::MetaProperty'), '... our property is a Perl::MetaProperty');
    is(%props<.prop2>.propType(), 'Int', '... the properties type is a string');
}


