#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

Testing Private methods

=cut

class Foo => {
    is => [ 'Perl6::Object' ],
    class => {
        methods => {
            '_foo_class' => sub { 'Foo::_foo' },
            'call_foo_class' => sub { CLASS()->_foo_class() }
        }
    },
    instance => {
        methods => {
            '_foo_instance' => sub { 'iFoo::_foo' },
            'call_foo_instance' => sub { SELF()->_foo_instance() }            
        }
    }
};

ok(!Foo->can('_foo_class'), '... can() does not expose private methods');
ok(!Foo->can('_foo_instance'), '... can() does not expose private methods');

dies_ok {
    Foo->_foo_class();
} '... this should die';

dies_ok {
    Foo->_foo_instance();
} '... this should die';

{
    my $value;
    lives_ok {
        $value = Foo->call_foo_class();
    } '... this should live';
    is($value, 'Foo::_foo', '... we got the right value');
}

{
    my $value;
    lives_ok {
        $value = Foo->new()->call_foo_instance();
    } '... this should live';
    is($value, 'iFoo::_foo', '... we got the right value');    
}    