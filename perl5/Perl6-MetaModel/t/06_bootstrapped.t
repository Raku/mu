#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 24;
use Test::Exception; 

require Perl6::MetaModel::Genesis;

# Now try to create a new class ....

my $Foo = $::Class->new('$:name' => 'Foo', '$:version' => '0.0.1');
is($Foo->name, 'Foo', '... Foo->name == Foo');
is($Foo->version, '0.0.1', '... Foo->version == 0.0.1');
is($Foo->authority, undef, '... Foo->authority == undef');

is($Foo->identifier, 'Foo-0.0.1', '... Foo->identifier == Foo-0.0.1');

lives_ok {
    $Foo->superclasses([ $::Object ]);
} '... set the superclass of Foo correctly';

is_deeply(
    $Foo->superclasses(),
    [ $::Object ],
    '... got the superclasses of Foo ok');

ok($Foo->isa('Foo'), '... Foo isa Foo');
ok($Foo->isa('Object'), '... Foo isa Object');
ok($Foo->isa('Class'), '... Foo isa Class');

# now try to create an instance from that class ...

my $iFoo = $Foo->new();
ok($iFoo->isa('Foo'), '... iFoo isa Foo');
ok($iFoo->isa('Object'), '... iFoo isa Object');
#ok(!$iFoo->isa('Class'), '... iFoo isa Class');

# Now try to create a new class ....

my $Bar = $::Class->new('$:name' => 'Bar', '$:version' => '0.0.1', '$:authority' => 'cpan:JRANDOM');
is($Bar->name, 'Bar', '... Bar->name == Bar');
is($Bar->version, '0.0.1', '... Bar->version == 0.0.1');
is($Bar->authority, 'cpan:JRANDOM', '... Bar->authority == cpan:JRANDOM');

is($Bar->identifier, 'Bar-0.0.1-cpan:JRANDOM', '... Bar->identifier == Bar-0.0.1-cpan:JRANDOM');

lives_ok {
    $Bar->superclasses([ $Foo ]);
} '... set the superclass of Bar correctly';

is_deeply(
    $Bar->superclasses(),
    [ $Foo ],
    '... got the superclasses of Bar ok');

ok($Bar->isa('Bar'), '... Bar isa Bar');
ok($Bar->isa('Foo'), '... Bar isa Foo');
ok($Bar->isa('Object'), '... Bar isa Object');
ok($Bar->isa('Class'), '... Bar isa Class');

# now try to create an instance from that class ...

my $iBar = $Bar->new();
ok($iBar->isa('Bar'), '... iBar isa Bar');
ok($iBar->isa('Foo'), '... iBar isa Foo');
ok($iBar->isa('Object'), '... iBar isa Object');
#ok(!$iBar->isa('Class'), '... not iBar isa Class');
