#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Exception;

do 'lib/gnosis.pl';

is(::opaque_instance_id($::Class), 1, '... our $::Class is the root');
is(::opaque_instance_class($::Class), $::Class, '... our $::Class is its own class');

lives_ok {
    $::Class->add_method('has_method' => ::make_method(sub {
        my ($self, $label) = @_;
        return ::opaque_instance_attrs($self)->{'%:methods'}->{$label} ? 1 : 0;
    }, $::Class));
} '... we can add a method';

ok($::Class->has_method('add_method'), '... and we can use it right away');
ok($::Class->has_method('has_method'), '... and our change has been applied');

dies_ok {
    $::Class->foo();
} '... no method found for this class';

dies_ok {
    $::Class->_foo();
} '... no private method found for this class';

# check to be sure the _class_dispatch routine 
# will dispatch to the superclass as well

my $Foo;
$Foo = ::create_class('$:name' => 'Foo');
::opaque_instance_attrs($Foo)->{'%:methods'}->{'hello'} = ::make_method(sub { 'Hello World' }, $Foo);

::opaque_instance_attrs($::Class)->{'@:superclasses'} = [ $Foo ];

is($::Class->hello(), 'Hello World', '... and we can dispatch to a superclass');
