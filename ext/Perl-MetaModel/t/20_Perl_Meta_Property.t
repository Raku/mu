#!/usr/bin/pugs

use v6;
use Test;

plan 17;

use Perl::Meta::Property;
use Perl::Meta::MetaClass;

my $prop = Perl::Meta::Property.new(type => 'Str', default => "Hello World");
ok($prop ~~ Perl::Meta::Property, '... we have a Perl::Meta::Property instance');

is($prop.type(), 'Str', '... the type is "Str"');
is($prop.default(), 'Hello World', '... the default is "Hello World"');

$! = undef;
dies_ok {
    $prop.default(10);
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^Incorrect value type for property default/, '... got the right error too');

my $mmc = Perl::Meta::MetaClass.new(:name<Class>);

$prop.associatedWith($mmc);
ok($prop.associatedWith() =:= $mmc, '... we are associated with $mmc');

$! = undef;
dies_ok {
    $prop.associatedWith(Perl::Meta::MetaClass.new(:name<Role>));
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^This property has already be associated with a something/, '... got the right error too');

$prop.type('Str');
is($prop.type(), 'Str', '... the type is still "Str"');
is($prop.default(), 'Hello World', '... the default is still "Hello World"');

$prop.type('Int');
is($prop.type(), 'Int', '... the type is now "Int"');
is($prop.default(), undef, '... the default is now undef');

$prop.type('Class');
lives_ok {
    $prop.default($mmc);
}, '... the default is set successfully';
is($prop.type(), 'Class', '... the type is now "Class"');
ok($prop.default() =:= $mmc, '... the default is now $mmc');

$! = undef;
dies_ok {
    $prop.default(Perl::Meta::MetaClass.new(:name<Role>));
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^Incorrect value type for property default/, '... got the right error too');
