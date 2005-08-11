#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 34;
use Data::Dumper;

use Perl6::MetaModel;
use Perl6::Object;

my $Person = Perl6::Class->new_class('Person' => {
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
                __('$:population' => ::dispatch(CLASS(), 'population') + 1);
                # we want this $class to possibly
                # be a subclass
                return ::dispatch($class, 'bless', (undef, %params));
            }
        }
    },
    instance => {
        attrs => [ '$.first_name', '$.last_name', [ '$.age' => { access => 'rw' } ] ],
        DESTROY => sub {
            __('$:population' => ::dispatch(CLASS, 'population') - 1);
        },
        methods => {
            full_name => sub {
                _('$.first_name') . ' ' . _('$.last_name');
            }
        }
    }
});

ok(::dispatch($Person, 'can', 'population'), '... Person->can(population)');

ok(::dispatch($Person, 'isa', ('Perl6::Object')), '... Person isa Perl6::Object');

is(::dispatch($Person, 'population'), 0, '... Person population is 0');

{
    my $p = ::dispatch($Person, 'create', ('$.first_name' => 'Steve', '$.last_name' => 'Little', '$.age' => 31));
    ok(::dispatch($p, 'isa', 'Person'), '... $p->isa(Person)');

    is(::dispatch($Person, 'population'), 1, '... population is 1');

    ok(::dispatch($p, 'can', 'first_name'), '... $p->can(first_name)');
    ok(::dispatch($p, 'can', 'last_name'), '... $p->can(last_name)');
    ok(::dispatch($p, 'can', 'full_name'), '... $p->can(full_name)');
    ok(::dispatch($p, 'can', 'age'), '... $p->can(age)');

    is(::dispatch($p, 'first_name'), 'Steve', '... got the right first name');
    is(::dispatch($p, 'last_name'), 'Little', '... got the right last name');
    is(::dispatch($p, 'full_name'), 'Steve Little', '... got the right full name');

    is(::dispatch($p, 'age'), 31, '... got the right age');
    
    # DESTROY needs to be called manually
    ::dispatch($p, 'DESTROY');
}

is(::dispatch($Person, 'population'), 0, '... Person population is back to 0 again');

# subclassing too...

my $Employee = Perl6::Class->new_class('Employee-0.0.1' => {
    is => [ 'Person' ],
    instance => {
        attrs => [ [ '$.job' => { access => 'rw' } ] ]
    }
});

ok(::dispatch($Employee, 'isa', 'Perl6::Object'), '... Employee isa Perl6::Object');
ok(::dispatch($Employee, 'isa', 'Person'), '... Employee isa Person');

is(::dispatch($Employee, 'population'), 0, '... Employee population is 0');
is(::dispatch($Person, 'population'), 0, '... Person population is 0');

{
    my $e = ::dispatch('Employee', 'create', (
        '$.first_name' => 'Steve', 
        '$.last_name'  => 'Little', 
        )
    );
    ok(::dispatch($e, 'isa', 'Employee'), '... $e->isa(Employee)');
    ok(::dispatch($e, 'isa', 'Person'), '... $e->isa(Person)');

    is(::dispatch($Employee, 'population'), 1, '... Employee population is 1');
    is(::dispatch($Person, 'population'), 1, '... Person population is 1 too (it is the Person class attribute)');

    ok(::dispatch($e, 'can', 'first_name'), '... $e->can(first_name)');
    ok(::dispatch($e, 'can', 'last_name'), '... $e->can(last_name)');
    ok(::dispatch($e, 'can', 'full_name'), '... $e->can(full_name)');
    ok(::dispatch($e, 'can', 'age'), '... $e->can(age)');
    ok(::dispatch($e, 'can', 'job'), '... $e->can(job)');    

    ::dispatch($e, 'age', (31));
    ::dispatch($e, 'job', ('Programmer'));

    is(::dispatch($e, 'first_name'), 'Steve', '... got the right first name');
    is(::dispatch($e, 'last_name'), 'Little', '... got the right last name');
    is(::dispatch($e, 'full_name'), 'Steve Little', '... got the right full name');

    is(::dispatch($e, 'age'), 31, '... got the right age');
    is(::dispatch($e, 'job'), 'Programmer', '... got the right job');   
    
    # DESTROY needs to be called manually    
    ::dispatch($e, 'DESTROY');     
}

is(::dispatch($Employee, 'population'), 0, '... Employee population is 0 again');
is(::dispatch($Person, 'population'), 0, '... and Person population is 0 again too');
