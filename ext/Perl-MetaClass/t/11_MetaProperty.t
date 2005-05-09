#!/usr/bin/pugs

use v6;
use Test;

use_ok('Perl::MetaProperty');
use_ok('Perl::MetaClass');

my $prop = Perl::MetaProperty::new('Str');

is($prop.propType(), 'Str', '... our property type is "Str"');
is($prop.propDefault(), undef, '... our property default is not defined');

lives_ok {
    $prop.propDefault('Testing default');
}, '... we set the property default successfully';

$prop.propType('List');
is($prop.propType(), 'List', '... our property type is now "List"');

is($prop.propDefault(), undef, '... our property default is now undefined since we changed types');

dies_ok {
    $prop.propDefault('Testing default');
}, '...  property default successfully';

lives_ok {
    $prop.propDefault([ 1, 2, 3 ]);
}, '... we set the property default successfully';

$prop.propType('Foo::Bar');
is($prop.propType(), 'Foo::Bar', '... our property type is now "Foo::Bar"');

is($prop.propDefault(), undef, '... our property default is now undefined since we changed types');

lives_ok {
    $prop.propDefault(Perl::MetaClass::new('Foo::Bar'));
}, '... we set the property default successfully';

my $prop2;
lives_ok {
    $prop2 = Perl::MetaProperty::new('Str', :default("Hello World"));
}, '... set our default in the constructor successfully'

is($prop2.propType(), 'Str', '... our property type is "Str"');
is($prop2.propDefault(), "Hello World", '... our property default is defined');
