#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;
use Test::Exception;

use Perl6::MetaModel;

=pod

This test is primarily focues on Roles and subroles

=cut

my $Foo = $::Role->new('$:name' => 'Foo');
isa_ok($Foo, 'Role');
# isa_ok($Foo, 'Foo'); << this should not work, but it does ??

ok($Foo->does('Foo'), '... our role does Foo');
ok(!$Foo->does('Baz'), '... our role does not do Baz');

my $Bar = $::Role->new('$:name' => 'Bar');
isa_ok($Bar, 'Role');

ok($Bar->does('Bar'), '... our role does Bar');
ok(!$Bar->does('Baz'), '... our role does not do Baz');

is_deeply(
    $Foo->subroles(),
    [],
    '... no subroles yet in Foo');

$Foo->subroles([ $Bar ]);

is_deeply(
    $Foo->subroles(),
    [ $Bar ],
    '... subroles added successfully');

ok($Foo->does('Foo'), '... our role still does Foo');
ok($Foo->does('Bar'), '... our role does Bar');
ok(!$Foo->does('Baz'), '... our role still does not do Baz');
