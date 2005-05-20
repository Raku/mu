#!/usr/bin/pugs

use v6;
use Test;

plan 10;

use Perl::Meta::Method;
use Perl::Meta::Class;

sub double ($self, Int $int) { $int*2 }

my $method = Perl::Meta::Method.new(code => \&double);
ok($method ~~ Perl::Meta::Method, '... we have a Perl::Meta::Method instance');

is($method.code().ref, 'Sub', '... the method is a "Sub"');
is($method.invoke('FakeInstance', 5), 10, '... invoking the method works');

my $mmc = Perl::Meta::Class.new(:name<Class>);

is($method.associatedWith(), undef, '... we are associated with nothing');

$method.associatedWith($mmc);
ok($method.associatedWith() =:= $mmc, '... we are associated with $mmc');

$! = undef;
dies_ok {
    $method.associatedWith(Perl::Meta::Class.new(:name<Role>));
}, '... incorrect type of default dies as expected';
like($!, rx:perl5/^This method has already be associated with a something/, '... got the right error too');

$method.removeAssociation();
is($method.associatedWith(), undef, '... we are associated with nothing again');

lives_ok {
    $method.associatedWith(Perl::Meta::Class.new(:name<Role>));
}, '... we can now associate with another class';

is($method.associatedWith().name(), 'Role', '... we are associated with a Role');
