#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 10;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

class Foo => {
    is => [ 'Perl6::Object' ],    
    class => {
        methods => {
            this_will_die => sub { SELF }
        }
    },
    instance => {
        attrs => [ '$.baz' ],
        BUILD => sub { _('$.baz' => 'Foo::baz') },
        methods => {
            bar => sub { _('$.baz') },
            foo => sub {
                SELF->bar();
            }
        }
    }
};

dies_ok {
    SELF();
} '... cannot call SELF outside of instance method';

can_ok('Foo', 'this_will_die');

dies_ok {
    Foo->this_will_die();
} '... cannot call SELF outside of instance method';

my $foo = Foo->new();
isa_ok($foo, 'Foo');

is($foo->baz(), 'Foo::baz', '... SELF worked correctly in the BUILD method');
is($foo->bar(), 'Foo::baz', '... SELF worked correctly in the instance methods');
is($foo->foo(), 'Foo::baz', '... SELF worked correctly in the instance methods in nested calls (in the same class)');


class Bar => {
    is => [ 'Perl6::Object' ],    
    instance => {
        attrs => [ '$.foo' ],
        BUILD => sub { _('$.foo' => Foo->new()) },
        methods => {
            bar => sub { 'Bar::bar' },
            baz => sub {
                my $val = SELF->foo()->bar();
                return "$val -> " . SELF->bar();
            }
        }
    }
};

my $bar = Bar->new();
isa_ok($bar, 'Bar');

isa_ok($bar->foo(), 'Foo');

is($bar->baz(), 'Foo::baz -> Bar::bar', '... SELF worked correctly in the instance methods in nested calls (in the same class)');
