#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 34;
use Data::Dumper;

use Perl6::MetaModel;
use Perl6::Object;

class 'Person-0.0.1-cpan:STEVAN' => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [ '$:population' ],
        methods => {
            population => sub {
                __('$:population') || 0;
            },
            create => sub {
                my ($class, %params) = @_;           
                # this CLASS should be the class it 
                # is defined in (i.e: Person)
                __('$:population' => CLASS->population() + 1);
                # we want this $class to possibly
                # be a subclass
                return $class->bless(undef, %params);
            }
        }
    },
    instance => {
        attrs => [ '$.first_name', '$.last_name', [ '$.age' => { access => 'rw' } ] ],
        DESTROY => sub {
            __('$:population' => CLASS->population() - 1);
        },
        methods => {
            full_name => sub {
                _('$.first_name') . ' ' . _('$.last_name');
            }
        }
    }
};

can_ok('Person', 'population');

ok(Person->isa('Perl6::Object'), '... Person isa Perl6::Object');

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

class 'Employee-0.0.1' => {
    is => [ 'Person' ],
    instance => {
        attrs => [ [ '$.job' => { access => 'rw' } ] ]
    }
};

ok(Employee->isa('Perl6::Object'), '... Employee isa Perl6::Object');
ok(Employee->isa('Person'), '... Employee isa Person');

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





