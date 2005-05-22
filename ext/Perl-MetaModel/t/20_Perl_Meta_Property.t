#!/usr/bin/pugs

use v6;
use Test;

plan 22;

use Perl::Meta::Property;
use Perl::Meta::Type;
use Perl::Meta::MetaClass;

my $prop = Perl::Meta::Property.new(type => MkType('Str'), default => "Hello World", :trait<rw>);
ok($prop ~~ Perl::Meta::Property, '... we have a Perl::Meta::Property instance');

is($prop.type().name(), 'Str', '... the type is "Str"');
is($prop.default(), 'Hello World', '... the default is "Hello World"');
is($prop.trait(), 'rw', '... the trait is "rw"');

$! = undef;
dies_ok {
    $prop.default(10);
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^Incorrect value type for property default/, '... got the right error too');

my $mmc = Perl::Meta::MetaClass.new(:name<Class>);

is($prop.associatedWith(), undef, '... we are associated with nothing');

$prop.associatedWith($mmc);
ok($prop.associatedWith() =:= $mmc, '... we are associated with $mmc');

$! = undef;
dies_ok {
    $prop.associatedWith(Perl::Meta::MetaClass.new(:name<Role>));
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^This property has already be associated with a something/, '... got the right error too');

$prop.removeAssociation();
is($prop.associatedWith(), undef, '... we are associated with nothing again');

lives_ok {
    $prop.associatedWith(Perl::Meta::MetaClass.new(:name<Role>));
}, '... we can now associate with another class';

is($prop.associatedWith().name(), 'Role', '... we are associated with a Role');

$prop.type(MkType('Str'));
is($prop.type().name(), 'Str', '... the type is still "Str"');
is($prop.default(), 'Hello World', '... the default is still "Hello World"');

$prop.type(MkType('Int'));
is($prop.type().name(), 'Int', '... the type is now "Int"');
is($prop.default(), undef, '... the default is now undef');

$prop.type(MkType('Class'));
lives_ok {
    $prop.default($mmc);
}, '... the default is set successfully';
is($prop.type().name(), 'Class', '... the type is now "Class"');
ok($prop.default() =:= $mmc, '... the default is now $mmc');

$! = undef;
dies_ok {
    $prop.default(Perl::Meta::MetaClass.new(:name<Role>));
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^Incorrect value type for property default/, '... got the right error too');
