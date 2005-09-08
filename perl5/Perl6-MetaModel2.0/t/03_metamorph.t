#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 72;
use Test::Exception;

do 'lib/metamorph.pl';

sub lives_ok_and_is (&$;$) {
    my ($block, $expected, $message) = @_;
    my $got;
    lives_ok {
        $got = $block->()
    } '... called the method succesfully';
    if (ref($expected)) {
        is_deeply($got, $expected, ($message || '... got the value we expected'));            
    }
    else {
        is($got, $expected, ($message || '... got the value we expected'));            
    }
}

## test all our defaults ...

lives_ok_and_is {
    $::Class->superclasses;
} [], '... got the superclasses we expected';

lives_ok_and_is {
    [ $::Class->MRO ];
} [ $::Class ], '... got the MRO we expected';

{
    my $dispatcher;
    lives_ok {
        $dispatcher = $::Class->dispatcher();
    } '... got the dispatcher ok';
    is(ref($dispatcher), 'CODE', '... the dispatcher is a CODE ref');
    is($dispatcher->(), $::Class, '... got the right element out of the dispatcher');
}

## now test some calculated values

sub lives_ok_and_ok (&$;$) {
    my ($block, $message, $not_ok) = @_;
    my $got;
    lives_ok {
        $got = $block->()
    } '... called the method succesfully';
    if ($not_ok) {
        ok(!$got, ($message || '... got the value we expected'));                    
    }
    else {
        ok($got, ($message || '... got the value we expected'));            
    }
}

lives_ok_and_ok {
    $::Class->is_a($::Class);
} '... $::Class->is_a(Class)';

lives_ok_and_ok {
    $::Class->is_a(my $Foo);
} '... not $::Class->is_a(Foo)', 1;

## now check the API using the has_method method

# check public methods
foreach my $method_name (qw(superclasses
                            subclasses
                            add_subclass
                            MRO
                            dispatcher
                            is_a
                            has_method
                            get_method
                            add_method
                            add_attribute
                            get_attribute
                            has_attribute
                            get_attribute_list
                            find_attribute_spec)) {
    lives_ok_and_ok {
        $::Class->has_method($method_name);
    } '... $::Class->has_method(' . $method_name . ')';
}

# check private methods
foreach my $method_name (qw(_merge
                            _make_dispatcher_iterator
                            _make_preorder_dispatcher
                            _make_breadth_dispatcher
                            _make_descendant_dispatcher
                            _make_ascendant_dispatcher
                            _get_method_table
                            _get_attribute_table)) {
    lives_ok_and_ok {
        $::Class->has_method($method_name, for => 'private');
    } '... $::Class->has_method(' . $method_name . ')';
}

my @attribute_name_list = ('@:MRO',        
                           '@:subclasses',      
                           '@:superclasses',    
                           '%:private_methods',  
                           '%:attributes',       
                           '%:methods',          
                           '%:class_attributes', 
                           '%:class_methods');

foreach my $attr_name (@attribute_name_list) {
    lives_ok_and_ok {
        $::Class->has_attribute($attr_name);
    } '... $::Class->has_attribute(' . $attr_name . ')';                    
}  

is_deeply(
    [ sort @attribute_name_list ], 
    [ sort $::Class->get_attribute_list() ], 
    '... got the same attribute list');
  
