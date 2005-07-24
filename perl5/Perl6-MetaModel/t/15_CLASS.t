#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

class Foo => {
    is => [ 'Perl6::Object' ],    
    class => {
        methods => {
            this_will_not_die => sub { CLASS }
        }
    },
    instance => {
        methods => {
            bar => sub {
                CLASS->this_will_not_die;
            },
            foo => sub {
                CLASS;
            }
        }
    }
};

dies_ok {
    CLASS();
} '... cannot call CLASS outside of method';

can_ok('Foo', 'this_will_not_die');

{
    my $val;
    lives_ok {
        $val = Foo->this_will_not_die();
    } '... CLASS can be called from a Class method';
    is($val, 'Foo', '... got the right value from CLASS too');
}

my $foo = Foo->new();
isa_ok($foo, 'Foo');

{
    my $val;
    lives_ok {
        $val = $foo->bar();
    } '... CLASS can be used to call Class methods';
    is($val, 'Foo', '... got the right value from CLASS too');
}

{
    my $val;
    lives_ok {
        $val = $foo->foo();
    } '... CLASS can be called from an instance method';
    is($val, 'Foo', '... got the right value from CLASS too');
}
