#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 26;
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

is($Foo->class, $::Class, '... Foo.class is Class');

$Foo->add_singleton_method('class_test' => ::make_method(sub { '$Foo::class_test' }));
is($Foo->class_test(), '$Foo::class_test', '... got the singleton class method');

my $eFoo = $Foo->class;
cmp_ok($Foo->class, '!=', $::Class, '... Foo.class is no longer Class');

$Foo->add_singleton_method('class_test2' => ::make_method(sub { '$Foo::class_test2' }));
is($Foo->class_test2(), '$Foo::class_test2', '... got the other singleton class method');

is($Foo->class, $eFoo, '... Foo.class is still eFoo (only one Eigenclass is created)');

my $foo = $Foo->new();
isa_ok($foo, 'Foo');

my $foo2 = $Foo->new();
isa_ok($foo2, 'Foo');

is($foo->class, $Foo, '... $foo.class is $Foo');
is($foo2->class, $Foo, '... $foo2.class is $Foo');

$foo->add_singleton_method('test' => ::make_method(sub { '$foo::test' }));
is($foo->test(), '$foo::test', '... the singleton method worked');

my $efoo = $foo->class;
cmp_ok($foo->class, '!=', $Foo, '... $foo.class is no longer $Foo');

is($foo2->class, $Foo, '... $foo2.class is still $Foo');

$foo->add_singleton_method('test2' => ::make_method(sub { '$foo::test2' }));
is($foo->test2(), '$foo::test2', '... the other singleton method worked');
is($foo->test(), '$foo::test', '... the first singleton method still worked');

is($foo->class, $efoo, '... $foo.class is still $eFoo (only one eigenclass is created)');

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

is($bar->class, $Bar, '... $bar.class is Bar');
is($bar2->class, $Bar, '... $bar.class is Bar');

$bar->add_singleton_method('baz' => ::make_method(sub { '$bar::baz' }));

is($bar->baz, '$bar::baz', '... got the right singleton method, overriding the classes method');
is($bar2->baz, 'Bar::baz', '... but still got the right method from the classes');

cmp_ok($bar->class, '!=', $Bar, '... $bar.class is no longer Bar');
is($bar2->class, $Bar, '... $bar.class is Bar');
