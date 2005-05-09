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

my $class = Perl::MetaClass::new('Role');

is($class.clsName(), 'Role', '... we got the right class name');

# Super Class

my $superclass = Perl::MetaClass::new('Package');

is($class.clsSuper(), undef, '... we do not have a superclass');
$class.clsSuper($superclass);
is($class.clsSuper().clsName(), 'Package', '... we now have a superclass');

# Sub Classes

{
    my @subclasses = $class.clsSubClasses();
    is(+@subclasses, 0, '... no subclasses yet');
}

my $subclass1 = Perl::MetaClass::new('Class');
$subclass1.clsSuper($class);

{
    my @subclasses = $class.clsSubClasses();
    is(+@subclasses, 1, '... we have 1 subclasses now');
    is(@subclasses[0].clsName(), 'Foo::Bar', '... this is our first subclass');
}

# Properties

{
    my %props = $class.clsProperties();
    is(+keys(%props), 0, '... we have no properties yet');
}

my $prop1 = Perl::MetaProperty::new('Str');
my $prop2 = Perl::MetaProperty::new('Int');

{
    my %props = $class.clsProperties('.prop1', $prop1, '.prop2', $prop2);
    my @keys = keys(%props);
    is(+@keys, 2, '... we have 2 properties now');
    is(@keys[0], '.prop1', '... we have the right property name');
    ok(%props<.prop1>.instance_isa('Perl::MetaProperty'), '... our property is a Perl::MetaProperty');
    is(%props<.prop1>.propType(), 'Str', '... the properties type is a string');

    is(@keys[1], '.prop2', '... we have the right property name');
    ok(%props<.prop2>.instance_isa('Perl::MetaProperty'), '... our property is a Perl::MetaProperty');
    is(%props<.prop2>.propType(), 'Int', '... the properties type is a string');
}


