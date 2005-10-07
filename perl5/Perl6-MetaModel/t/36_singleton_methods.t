#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;
use Test::Exception;

use Perl6::MetaModel;

=pod

This is an example of Ruby-style singleton methods using the
Perl6::MetaModel. To the best of my knowledge, this is actually
how it is implemented in Ruby as well. 

Here is a link to a description of how this works in Ruby:

http://www.rubygarden.org/ruby?ClassMethods/Discussion

=cut

my $Foo = $::Class->new('$:name' => 'Foo');
$Foo->superclasses([ $::Object ]);
isa_ok($Foo, 'Class');

$Foo->add_singleton_method('class_test' => ::make_method(sub { '$Foo::class_test' }));

is($Foo->class_test(), '$Foo::class_test', '... got the singleton class method');

my $foo = $Foo->new();
isa_ok($foo, 'Foo');

my $foo2 = $Foo->new();
isa_ok($foo2, 'Foo');

$foo->add_singleton_method('test' => ::make_method(sub { '$foo::test' }));

is($foo->test(), '$foo::test', '... the singleton method worked');

dies_ok {
    $foo2->test();
} '... the singleton method is only for $foo';

# test it again,...

my $Bar = class 'Bar' => {
    is => [ $::Object ],
    methods => {
        'baz' => sub { 'Bar::baz' }
    }
};
isa_ok($Bar, 'Class');

my $bar = $Bar->new();
isa_ok($bar, 'Bar');

my $bar2 = $Bar->new();
isa_ok($bar2, 'Bar');

$bar->add_singleton_method('baz' => ::make_method(sub { '$bar::baz' }));

is($bar->baz, '$bar::baz', '... got the right singleton method, overriding the classes method');
is($bar2->baz, 'Bar::baz', '... but still got the right method from the classes');
