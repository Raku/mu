#!/usr/bin/pugs

use v6;
use Test;

use_ok('Perl::MetaClass');
use_ok('Perl::MetaProperty');

# --------------------------------------------------------------
# This test attempts to model the following class.
# --------------------------------------------------------------
#
#           Bar           <- Super Class
#            |
#           Foo           <- Class
#            |
#    [ .prop1 .prop2 ]    <- Properties
#            |
#   [ method1 method2 ]   <- Methods
#            |
#      +-----+----+
#      |          |
#   Foo::Bar   Bar::Baz   <- SubClasses
#
# --------------------------------------------------------------

my $class = Perl::MetaClass::new('Foo');

is($class.clsName(), 'Foo', '... we got the right class name');

# Super Class

my $superclass = Perl::MetaClass::new('Bar');

is($class.clsSuper(), undef, '... we do not have a superclass');
$class.clsSuper($superclass);
is($class.clsSuper().clsName(), 'Bar', '... we now have a superclass');

# Sub Classes

{
    my @subclasses = $class.clsSubClasses();
    is(+@subclasses, 0, '... no subclasses yet');
}

my $subclass1 = Perl::MetaClass::new('Foo::Bar');
$subclass1.clsSuper($class);

{
    my @subclasses = $class.clsSubClasses();
    is(+@subclasses, 1, '... we have 1 subclasses now');
    is(@subclasses[0].clsName(), 'Foo::Bar', '... this is our first subclass');
}

my $subclass2 = Perl::MetaClass::new('Bar::Baz');
$subclass2.clsSuper($class);

{
    my @subclasses = $class.clsSubClasses();
    is(+@subclasses, 2, '... we have 2 subclasses now');
    is(@subclasses[0].clsName(), 'Foo::Bar', '... this is our first subclass');
    is(@subclasses[1].clsName(), 'Bar::Baz', '... this is our second subclass');    
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


