#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 31;
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
                return $class->bless(undef, %params);
            }
        }
    },
    instance => {
        attrs => [ '$.first_name', '$.last_name', '$.age' ],
        DESTROY => sub {
            my ($self) = @_;
            my $class = ref($self);
            $class->set_class_value('$:population' => $class->population() - 1);
        },
        methods => {
            full_name => sub {
                my ($self) = @_;
                $self->get_value('$.first_name') . ' ' . $self->get_value('$.last_name');
            }
        }
    }
};

can_ok('Person', 'population');

is(Person->population(), 0, '... Person population is 0');

{
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
}

is(Person->population(), 0, '... Person population is back to 0 again');

# subclassing too...

class Employee => {
    is => [ 'Person' ],
    instance => {
        attrs => [ '$.job' ]
    }
};

is(Employee->population(), 0, '... Employee population is 0');
is(Person->population(), 0, '... Person population is 0');

{
    my $e = Employee->create(
        '$.first_name' => 'Steve', 
        '$.last_name'  => 'Little', 
    );
    isa_ok($e, 'Employee');
    isa_ok($e, 'Person');

    is(Employee->population(), 1, '... Employee population is 1');
    is(Person->population(), 1, '... Person population is 1 too (it is the Person class attribute)');

    can_ok($e, 'first_name');
    can_ok($e, 'last_name');
    can_ok($e, 'full_name');
    can_ok($e, 'age');
    can_ok($e, 'job');    

    $e->age(31);
    $e->job('Programmer');

    is($e->first_name(), 'Steve', '... got the right first name');
    is($e->last_name(), 'Little', '... got the right last name');
    is($e->full_name(), 'Steve Little', '... got the right full name');

    is($e->age(), 31, '... got the right age');
    is($e->job(), 'Programmer', '... got the right job');    
}

is(Employee->population(), 0, '... Employee population is 0 again');
is(Person->population(), 0, '... and Person population is 0 again too');





