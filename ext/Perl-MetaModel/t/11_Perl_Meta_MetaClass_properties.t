#!/usr/bin/pugs

use v6;
use Test;

plan 10;

use Perl::Meta::Class;
use Perl::Meta::Property;

=pod

This class tests property assignment and removal

=cut

my $mmc = Perl::Meta::Class::new('Class');

{
    my @property_labels = $mmc.propertyLabels();
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
$mmc.addProperty('rw', $rw_prop);
$mmc.addProperty('name', $name_prop);

# however, these 

{
    my @property_labels = sort $mmc.propertyLabels();
    is(+@property_labels, 2, '... we have 2 property labels');
    is(@property_labels[0], 'name', '... the first is name');
    is(@property_labels[1], 'rw', '... the second is rw');
}

{
    my %properties = sort $mmc.properties();
    is(+%properties, 2, '... we have 2 properties');
    ok(%properties{'rw'} =:= $rw_prop, '... the first is $rw_prop');
    ok(%properties{'name'} =:= $name_prop, '... the second is $name_prop');
}

my $removed_prop = $mmc.removeProperty('name');
ok($removed_prop =:= $name_prop, '... removed $name_prop');

{
    my %properties = sort $mmc.properties();
    is(+%properties, 1, '... we have 1 property'); 
    ok(%properties{'rw'} =:= $rw_prop, '... the first is $rw_prop');     
}
