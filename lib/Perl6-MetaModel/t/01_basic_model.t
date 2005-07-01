#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Data::Dumper;

use Perl6::MetaModel;

class Person => {
    class => {
        attrs => [ '$:population' ],
        methods => {
            population => sub {
                my ($class) = @_;              
                $class->get_class_value('$:population') || 0;
            },
            create => sub {
                my ($class, %params) = @_;                
                $class->set_class_value('$:population' => $class->population() + 1);
                return $class->new(%params);
            }
        }
    },
    instance => {
        attrs => [ '$.first_name', '$.last_name', '$.age' ],
        methods => {
            full_name => sub {
                my ($self) = @_;
                $self->get_value('$.first_name') . ' ' . $self->get_value('$.last_name');
            }
        }
    }
};

can_ok('Person', 'population');

is(Person->population(), 0, '... population is 0');

my $p = Person->create('$.first_name' => 'Steve', '$.last_name' => 'Little', '$.age' => 31);
isa_ok($p, 'Person');

is(Person->population(), 1, '... population is 1');

can_ok($p, 'first_name');
can_ok($p, 'last_name');
can_ok($p, 'full_name');
can_ok($p, 'age');

is($p->first_name(), 'Steve', '... got the right first name');
is($p->last_name(), 'Little', '... got the right last name');
is($p->full_name(), 'Steve Little', '... got the right full name');

is($p->age(), 31, '... got the right age');

