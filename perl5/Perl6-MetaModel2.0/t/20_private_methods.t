#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Exception;

use Perl6::MetaModel;

=pod

Testing Private methods

=cut

my $Foo = class 'Foo' => {
    'is' => [ $::Object ],
    'class_methods' => {
        '_foo_class'     => sub { 'Foo::_foo' },
        'call_foo_class' => sub { $::CLASS->class::_foo_class() }
    },
    'methods' => {
        '_foo_instance' => sub { 'iFoo::_foo' },
        'call_foo_instance' => sub { $::SELF->_foo_instance() }            
    }
};

ok(!Foo->can('_foo_instance'), '... can() does not expose private methods');

dies_ok {
    $Foo->class::_foo_class();
} '... this should die';

dies_ok {
    $Foo->_foo_instance();
} '... this should die';

{
    my $value;
    lives_ok {
        $value = $Foo->class::call_foo_class();
    } '... this should live';
    is($value, 'Foo::_foo', '... we got the right value');
}

{
    my $value;
    lives_ok {
        $value = $Foo->new()->call_foo_instance();
    } '... this should live';
    is($value, 'iFoo::_foo', '... we got the right value');    
}   


my $Bar = class Bar => {
    'is' => [ $::Object ],
    'class_methods' => {
        'call_foo_class' => sub { $Foo->class::_foo_class() }
    }
};

# XXX - 
# I am not sure this throws the right error,.. it does die though :)

dies_ok {
    $Bar->class::call_foo_class();
} '... this should die';


