#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::Attribute;

=pod

This test file just checks the details of the Perl6::Attribute class

=cut

my $foo_prop = Perl6::Attribute->new('Foo' => '$.foo');
isa_ok($foo_prop, 'Perl6::Attribute');

is($foo_prop->accessor_name(), 'foo', '... our attributes accessor name is "foo"');
ok($foo_prop->is_public(), '... our foo attribute is public');
ok(!$foo_prop->is_private(), '... our foo attribute is not private');

ok(!$foo_prop->is_array(), '... our foo attribute is not an array');

my $bar_prop = Perl6::Attribute->new('Bar' => '$:bar');
isa_ok($bar_prop, 'Perl6::Attribute');

is($bar_prop->accessor_name(), 'bar', '... our attributes accessor name is "bar"');
ok(!$bar_prop->is_public(), '... our bar attribute is not public');
ok($bar_prop->is_private(), '... our bar attribute is private');

ok(!$bar_prop->is_array(), '... our bar attribute is not an array');

my $baz_prop = Perl6::Attribute->new('Baz' => '@.baz', 'Foo::Bar');
isa_ok($baz_prop, 'Perl6::Attribute');

is($baz_prop->accessor_name(), 'baz', '... our attributes accessor name is "baz"');
ok($baz_prop->is_public(), '... our baz attribute is public');
ok(!$baz_prop->is_private(), '... our baz attribute is not private');
is($baz_prop->type(), 'Foo::Bar', '... got the right type');

ok($baz_prop->is_array(), '... our baz attribute is an array');