#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 32;

use Perl6::MetaModel;

=pod

class Person-0.0.1-cpan:STEVAN {
    our $.population;
    
    method population (Class $c:) { $.population }
    
    method create (Class $c: *%params) {
        $.population = $.population + 1;
        return $c.bless(undef, %params);
    }
    
    submethod DESTROY {
        $.population = $.population - 1;
    }     
    
    has $.first_name;
    has $.last_name;
    has $.age is rw;
    
    method full_name () { $.first_name ~ ' ' ~ $.last_name }
}

=cut

my $Person = class 'Person-0.0.1-cpan:STEVAN' => {
    is => [ $::Object ],
    class_attributes => [ '$.population' ],
    class_methods => {
        population => sub {
            $::CLASS->FETCH('$.population') || 0;
        },
        create => sub {
            my ($class, %params) = @_;           
            # this CLASS should be the class it 
            # is defined in (i.e: Person)
            $::CLASS->STORE('$.population' => $::CLASS->population() + 1);
            # we want this $class to possibly
            # be a subclass
            return $class->bless(undef, %params);
        }
    },
    attributes => [ '$.first_name', '$.last_name', '$.age' ],
    submethods => {
        DESTROY => sub {
            $::CLASS->STORE('$.population' => $::CLASS->population() - 1);
        }
    },
    methods => {
        first_name => sub { _('$.first_name') },
        last_name  => sub { _('$.last_name')  },
        age => sub { 
            shift;
            _('$.age' => shift) if @_;
            _('$.age');
        },                
        full_name => sub { 
            _('$.first_name') . ' ' . _('$.last_name') 
        }
    }
};

is($Person->population(), 0, '... Person population is 0');

{
    my $p = $Person->create('$.first_name' => 'Steve', '$.last_name' => 'Little', '$.age' => 31);
    isa_ok($p, 'Person');

    is($Person->population(), 1, '... population is 1');

    can_ok($p, 'first_name');
    can_ok($p, 'last_name');
    can_ok($p, 'full_name');
    can_ok($p, 'age');

    is($p->first_name(), 'Steve', '... got the right first name');
    is($p->last_name(), 'Little', '... got the right last name');
    is($p->full_name(), 'Steve Little', '... got the right full name');

    is($p->age(), 31, '... got the right age');
}

is($Person->population(), 0, '... Person population is back to 0 again');

# subclassing too...
=pod

class Employee-0.0.1 is Person {
    has $.job is rw;
}

=cut

my $Employee = class 'Employee-0.0.1' => {
    is => [ $Person ],
    attributes => [ '$.job' ],
    methods => {
        job => sub {
            shift;
            _('$.job' => shift) if @_;
            _('$.job');            
        }
    }
};

ok($Employee->isa('Object'), '... Employee isa Object');
ok($Employee->isa('Person'), '... Employee isa Person');

is($Employee->population(), 0, '... Employee population is 0');
is($Person->population(), 0, '... Person population is 0');

{
    my $e = $Employee->create(
        '$.first_name' => 'Steve', 
        '$.last_name'  => 'Little', 
    );
    isa_ok($e, 'Employee');
    isa_ok($e, 'Person');

    is($Employee->population(), 1, '... Employee population is 1');
    is($Person->population(), 1, '... Person population is 1 too (it is the Person class attribute)');

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

is($Employee->population(), 0, '... Employee population is 0 again');
is($Person->population(), 0, '... and Person population is 0 again too');
