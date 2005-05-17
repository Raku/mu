#!/usr/bin/pugs

use v6;
use Test;

plan 10;

use Perl::Meta::MetaClass;
use Perl::Meta::Property;

=pod

This class tests property assignment and removal

=cut

my $mmc = Perl::Meta::MetaClass::new('Class');

{
    my @property_labels = $mmc.propertyLabels();
    is(+@property_labels, 0, '... we have no property labels yet'); 
}

my $prop1 = Perl::Meta::Property.new(:type<Str>);
my $prop2 = Perl::Meta::Property.new(:type<Int>);

$mmc.addProperty('prop1', $prop1);
$mmc.addProperty('prop2', $prop2);

{
    my @property_labels = sort $mmc.propertyLabels();
    is(+@property_labels, 2, '... we have 2 property labels'); 
    is(@property_labels[0], 'prop1', '... the first is prop1'); 
    is(@property_labels[1], 'prop2', '... the second is prop2');     
}

{
    my %properties = sort $mmc.properties();
    is(+%properties, 2, '... we have 2 properties'); 
    ok(%properties{'prop1'} =:= $prop1, '... the first is $prop1'); 
    ok(%properties{'prop2'} =:= $prop2, '... the second is $prop2');     
}

my $removed_prop = $mmc.removeProperty('prop2');
ok($removed_prop =:= $prop2, '... removed $prop2');

{
    my %properties = sort $mmc.properties();
    is(+%properties, 1, '... we have 1 property'); 
    ok(%properties{'prop1'} =:= $prop1, '... the first is $prop1');     
}
