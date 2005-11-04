#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::MetaModel::Bootstrap');

my $Foo = $::Class->send('new');
isa_ok($Foo, 'opaque');

is($Foo->id->greater_than(num->new(2)), $bit::TRUE, '... this is greater than the second instance');
is($Foo->class, $::Class, 'Foo is an instance of Class');

$Foo->send('superclasses' => list->new($::Object));

END {
    my $temp = $bit::TRUE;
    $temp = $::Object;
}