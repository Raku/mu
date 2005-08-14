#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 32;

BEGIN { do "lib/genesis.pl" };

# Utility to test that "No method found" error is raised

sub fails_ok (&$) {
    my ($code, $desc) = @_;
    local $@; eval { &$code };
    like($@, qr/No method found/, $desc);
}

# Begins testing

is($::Class->id, 1, '... $::Class is the first id');
is($::Class->class, $::Class, '... $::Class refs to itself');
is($::Class->name, 'Class', '... $::Class got the right method return value');
is_deeply(
    $::Class->superclass, 
    [ $::Object ], 
    '... $::Class is now a subclass of $::Object');
is_deeply(
    [ $::Class->class_precendence_list ], 
    [ $::Class, $::Object ], 
    '... $::Class class_precendence_list');

is($::Object->id, 2, '... $::Object is the second id');
is($::Object->class, $::Class, '... $::Object class slot is $::Class');
is($::Object->name, 'Object', '... $::Object got the right method return value');
is_deeply(
    $::Object->superclass, 
    [], 
    '... $::Object got the right method return value');
is_deeply(
    [ $::Object->class_precendence_list ], 
    [ $::Object ], 
    '... $::Object class_precendence_list');

## make class

my $Foo = $::Class->new(
    '$:name'         => 'Foo',
    '@:superclasses' => [ $::Object ],
    '%:methods'      => {
        'foo' => sub ($) { 'Foo->foo' },
        'bar' => sub ($) { 'Foo->bar' },
    },
);

is($Foo->id, 3, '... $Foo is the fourth id');
is($Foo->class, $::Class, '... $Foo refs to metaclass');
is($Foo->name, 'Foo', '... $Foo got the right method return value');
is_deeply(
    $Foo->superclass, 
    [ $::Object ], 
    '... $Foo got the right method return value');
is_deeply(
    [ $Foo->class_precendence_list ], 
    [ $Foo, $::Object ], 
    '... $Foo class_precendence_list');

fails_ok { $Foo->bar } '... metaclass calling instance method fails';

## make instances

my $iFoo = $Foo->new;
is($iFoo->id, 4, '... $iFoo is the fourth id');
is($iFoo->class, $Foo, '... $iFoo refs to $Foo');

# try to call the Class method
fails_ok { $iFoo->name } '... instance calling metaclass method fails';

is($iFoo->foo, 'Foo->foo', '... $iFoo got the right method return value');
is($iFoo->bar, 'Foo->bar', '... $iFoo got the right method return value');

## make subclasses

my $Bar = $::Class->new(
    '$:name'         => 'Bar',
    '@:superclasses' => [ $Foo ],
    '%:methods'      => {
        'bar' => sub ($) { 'Bar->bar' },
        'baz' => sub ($) { 'Bar->baz' },
    },
);

is($Bar->id, 5, '... $Bar is the fifth id');
is($Bar->class, $::Class, '... $Bar refs to metaclass');
is($Bar->name, 'Bar', '... $Bar got the right method return value');
is_deeply(
    $Bar->superclass, 
    [ $Foo ], 
    '... $Bar got the right method return value');
is_deeply(
    [ $Bar->class_precendence_list ], 
    [ $Bar, $Foo, $::Object ], 
    '... $Bar class_precendence_list');

## make instances of subclasses

my $iBar = $Bar->new;
is($iBar->id, 6, '... $iBar is the sixth id');
is($iBar->class, $Bar, '... $iBar refs to $Bar');

# try to call the Class method
fails_ok { $iBar->name } '... instance calling metaclass method fails';

is($iBar->foo, 'Foo->foo', '... $iBar calls superclass foo');
is($iBar->bar, 'Bar->bar', '... $iBar calls overridden bar');
is($iBar->baz, 'Bar->baz', '... $iBar calls new method baz');
