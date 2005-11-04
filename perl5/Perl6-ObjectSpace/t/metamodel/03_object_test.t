#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::MetaModel::Bootstrap');

isa_ok($::Object, 'opaque');
is($::Object->id->equal_to(num->new(2)), $bit::TRUE, '... this is the second instance');
is($::Object->class, $::Class, 'Object is an instance of Class');

is($::Object->send('is_a' => $::Object), $bit::TRUE, '... Object is_a Object');    

my $superclasses = $::Object->send('superclasses');
isa_ok($superclasses, 'list');
is($superclasses->is_empty, $bit::TRUE, '... the superclass list is currently empty');

my $MRO = $::Object->send('MRO');
isa_ok($MRO, 'list');
is($MRO->is_empty, $bit::FALSE, '... the MRO list is not empty');
is($MRO->length->equal_to(num->new(1)), $bit::TRUE, '... the MRO list has one item in it');
is($MRO->fetch(num->new(0)), $::Object, '... and that item is Object');

foreach my $method (qw(
                    new bless CREATE BUILDALL BUILD
                    add_method get_method get_method_list has_method remove_method
                    add_attribute get_attribute get_attributes get_attribute_list has_attribute
                    superclasses subclasses add_subclass MRO
                    DESTROYALL
                    )) {
    is($::Object->send('can' => symbol->new($method))->is_nil, 
         $bit::FALSE, 
         '... Object->can(' . $method . ')');
}

foreach my $method (qw(foo bar baz)) {
    is($::Object->send('can' => symbol->new($method))->is_nil, 
         $bit::TRUE, 
         '... !Object->can(' . $method . ')');
}

my $i = $::Object->send('new');
isa_ok($i, 'opaque');

is($i->id->equal_to(num->new(3)), $bit::TRUE, '... our id is the third id');
is($i->class, $::Object, '... our class is Object');

foreach my $method (qw(
                    BUILDALL BUILD
                    can 
                    id class
                    )) {
    is($i->send('can' => symbol->new($method))->is_nil, 
         $bit::FALSE, 
         '... $i->can(' . $method . ')');
}


foreach my $method (qw(foo bar baz)) {
    is($i->send('can' => symbol->new($method))->is_nil, 
         $bit::TRUE, 
         '... !$i->can(' . $method . ')');
}

END {
    my $temp = $::Class;
}
