#!/usr/bin/pugs

use v6;
use Test;

plan 29;

use Perl::Meta::Class;
use Perl::Meta::Property;

=pod

This class tests property assignment and removal

=cut

my $class = Perl::Meta::Class::new('Class');

{
    my @property_labels = $class.propertyLabels();
    is(+@property_labels, 0, '... we have no property labels yet'); 
}

my $rw_prop = Perl::Meta::Property.new(:type<Bool>);
my $name_prop = Perl::Meta::Property.new(:type<Str>);

# note that properties of classes are called Class traits.

# So, when you *define* a trait, it modifies the MetaModel to add the
# possibility of the property.

# When you *attach* a trait to a class at compile time, it modifies
# the Model.

# When you *attach* a trait to a class at run time, it modifies the
# Model, possibly by making new minimal sub-class/role objects, and
# then marks that object as being in that new minimal sub-class.

# these Class traits are mentioned in the synopses..
$class.addProperty('rw', $rw_prop);
$class.addProperty('name', $name_prop);

ok($class.isPropertySupported('rw'), '... the "rw" property is supported');
ok($class.isPropertySupported('name'), '... the "name" property is supported');
ok(!$class.isPropertySupported('foo'), '... the "foo" property is not supported');

{
    my @property_labels = sort $class.propertyLabels();
    is(+@property_labels, 2, '... we have 2 property labels');
    is(@property_labels[0], 'name', '... the first is name');
    is(@property_labels[1], 'rw', '... the second is rw');
}

{
    my %properties = sort $class.properties();
    is(+%properties, 2, '... we have 2 properties');
    ok(%properties{'rw'} =:= $rw_prop, '... the first is $rw_prop');
    ok(%properties{'name'} =:= $name_prop, '... the second is $name_prop');
}

my $removed_prop = $class.removeProperty('name');
ok($removed_prop =:= $name_prop, '... removed $name_prop');

ok($class.isPropertySupported('rw'), '... the "rw" property is supported');
ok(!$class.isPropertySupported('name'), '... the "name" property is no longer supported');
ok(!$class.isPropertySupported('foo'), '... the "foo" property is not supported');

{
    my %properties = sort $class.properties();
    is(+%properties, 1, '... we have 1 property'); 
    ok(%properties{'rw'} =:= $rw_prop, '... the first is $rw_prop');     
}

my $ts_class = Perl::Meta::Class::new('ThreadSafeClass');
$ts_class.superclass($class);

my $semaphore_prop = Perl::Meta::Property.new(:type<Semaphore>);
my $name_prop_2 = Perl::Meta::Property.new(:type<Str>);

$ts_class.addProperty('semaphore', $semaphore_prop);
$ts_class.addProperty('name', $name_prop_2);

ok($ts_class.isPropertySupported('rw'), '... the "rw" property is supported by ThreadSafeClass');

ok($ts_class.isPropertySupported('name'), '... the "name" property is supported by ThreadSafeClass');
ok(!$class.isPropertySupported('name'), '... the "name" property is not supported by Class');

$class.addProperty('name', $name_prop);
ok($class.isPropertySupported('name'), '... the "name" property is now supported by Class (again)');

ok(!$ts_class.isPropertySupported('foo'), '... the "foo" property is not supported by ThreadSafeClass');
ok($ts_class.isPropertySupported('semaphore'), '... the "semaphore" property is supported by ThreadSafeClass');

{
    my %props = $ts_class.allProperties();
    is(+%props.keys, 3, '... we got 3 properties in ThreadSafeClass');
    ok(%props{'rw'} =:= $rw_prop, '... the "rw" prop is correct in ThreadSafeClass');
    ok(%props{'name'} =:= $name_prop_2, '... the "name" prop is correct in ThreadSafeClass');
    ok(%props{'semaphore'} =:= $semaphore_prop, '... the "semaphore" prop is correct in ThreadSafeClass');        
}

{
    my %props = $class.allProperties();
    is(+%props.keys, 2, '... we got 2 properties in Class');
    ok(%props{'rw'} =:= $rw_prop, '... the "rw" prop is correct in Class');
    ok(%props{'name'} =:= $name_prop, '... the "name" prop is correct in Class');
}
