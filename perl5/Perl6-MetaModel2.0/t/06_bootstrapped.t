#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 14;
use Test::Exception; 

do 'lib/genesis.pl';

# Now try to create a new class ....

my $Foo = $::Class->class::new('$:name' => 'Foo', '$:version' => '0.0.1');
is($Foo->name, 'Foo', '... Foo->name == Foo');
is($Foo->version, '0.0.1', '... Foo->version == 0.0.1');

lives_ok {
    $Foo->superclasses([ $::Object ]);
} '... set the superclass of Foo correctly';

is_deeply(
    $Foo->superclasses(),
    [ $::Object ],
    '... got the superclasses of Foo ok');

ok($Foo->class::isa('Foo'), '... Foo isa Foo');

# now try to create an instance from that class ...

my $iFoo = $Foo->class::new();
ok($iFoo->isa('Foo'), '... iFoo isa Foo');

# Now try to create a new class ....

my $Bar = $::Class->class::new('$:name' => 'Bar', '$:version' => '0.0.1');
is($Bar->name, 'Bar', '... Bar->name == Bar');
is($Bar->version, '0.0.1', '... Bar->version == 0.0.1');

lives_ok {
    $Bar->superclasses([ $Foo ]);
} '... set the superclass of Bar correctly';

is_deeply(
    $Bar->superclasses(),
    [ $Foo ],
    '... got the superclasses of Bar ok');

ok($Bar->class::isa('Bar'), '... Bar isa Bar');
ok($Bar->class::isa('Foo'), '... Bar isa Foo');

# now try to create an instance from that class ...

my $iBar = $Bar->class::new();
ok($iBar->isa('Bar'), '... iBar isa Bar');
ok($iBar->isa('Foo'), '... iBar isa Foo');
