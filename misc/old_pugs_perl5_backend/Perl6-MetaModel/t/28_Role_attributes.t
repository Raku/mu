#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 10;
use Test::Exception;

use Perl6::MetaModel;

=pod

This test is primarily focues on Role attributes

=cut

my $Foo = $::Role->new('$:name' => 'Foo');
isa_ok($Foo, 'Role');
ok(!$Foo->isa('Foo'), '... $Foo is not a Foo'); 

is_deeply(
    [ $Foo->get_attribute_list ],
    [],
    '... no $Foo attribute yet');

my $foo_attr = ::make_attribute('$.foo');
$Foo->add_attribute('$.foo' => $foo_attr);

ok($Foo->has_attribute('$.foo'), '... $Foo.has_attribute($.foo)');
is($Foo->get_attribute('$.foo'), $foo_attr, '... $Foo.get_attribute($.foo)');

ok(!$Foo->has_attribute('$.bar'), '... not $Foo.has_attribute($.bar)');

is_deeply(
    [ $Foo->get_attribute_list ],
    [ '$.foo' ],
    '... got the list of $Foo attributes');

my $bar_attr = ::make_attribute('$.bar');
$Foo->add_attribute('$.bar' => $bar_attr);

ok($Foo->has_attribute('$.bar'), '... $Foo.has_attribute($.bar)');
is($Foo->get_attribute('$.bar'), $bar_attr, '... $Foo.get_attribute($.bar)');

is_deeply(
    [ sort $Foo->get_attribute_list ],
    [ '$.bar', '$.foo' ],
    '... got the list of $Foo attributes');
