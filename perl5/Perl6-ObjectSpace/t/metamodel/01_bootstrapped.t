#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MetaModel::Bootstrap');


foreach my $method_name (qw(
    add_method
    has_method
    remove_method
    get_method
    get_method_list
    new
    bless
    CREATE
    BUILDALL
    BUILD
    DESTROYALL
    id 
    class
    superclasses
    subclasses
    add_subclass
    _merge
    MRO
    dispatcher
    _make_dispatcher_iterator
    _make_descendant_dispatcher
    _make_ascendant_dispatcher    
    is_a
    add_attribute
    )) {
    is($::Class->send('has_method' => (symbol->new($method_name))),
       $bit::TRUE,
       '... we have the methods "' . $method_name . '" in our class');    
}

END {
    my $temp = $bit::TRUE;
    $temp = $::Class;
}