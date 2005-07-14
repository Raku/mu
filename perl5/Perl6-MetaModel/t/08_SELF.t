#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;
use Test::Exception;

use Perl6::MetaModel;

class Foo => {
    class => {
        methods => {
            this_will_die => sub { SELF }
        }
    },
    instance => {
        attrs => [ '$.baz' ],
        BUILD => sub {
            SELF->set_value('$.baz' => 'Foo::baz');            
        },
        methods => {
            bar => sub {
                SELF->get_value('$.baz');
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

is($foo->bar(), 'Foo::baz', '... SELF worked correctly in the instance methods');
