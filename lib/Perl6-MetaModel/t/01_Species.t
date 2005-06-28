#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Data::Dumper;

use Perl6::MetaModel;

=pod

This test file checks basic Object/Class/Kind interactions:

=over 4

=item Kind's are essentially Class instance factories

=item Class's define instance methods

=item Class's can init instance data/attributes

=item Class's can hold instance data/attributes

=item Class attributes will generate rw accessors for public attributes

=item Kind's define class methods

=item Kind's can init class data/attributes 

=item Kind's can hold class data/attributes 

=back

=cut


# class Species kind Object extends Object {
#     class has $animals;
#     class method is_extinct { ... }
# }

class Species => {
    kind => {
        attrs => [ '@:animals' ],
        init  => sub {
            my ($self) = @_;
            $self->set_value('@:animals' => []);
        },
        methods => {
            add_animal => sub {
                my ($meta, $animal) = @_;
                push @{$meta->get_value('@:animals')} => $animal;
            },
            is_extinct => sub {
                scalar @{(shift)->get_value('@:animals')} == 0 ? 1 : 0
            }
        }
    }
};


# class Bird kind Object extends Object {
#    has $wings;
#    has $beak;
#    method fly { ... }
# }

class Bird => {
    class => {
        attrs   => [ '@:wings', '$:beak' ],
        init    => sub {
            my ($self) = @_;
            $self->set_value('@:wings' => [ 0, 0 ]);
        },
        methods => {
            fly => sub {
                my $self = shift;
                return "The ($self) is Flying!"
            }
        }
    }
};

# class Eagle kind Species extends Bird {
#    has $name;
#    class method make { ... }
# }

class Eagle => {
    kind_of => 'Species',
    extends => 'Bird',
    class   => {
        attrs   => [ '$.name' ],
    },
    kind => {
        methods => {
            make => sub {
                my ($class, $name) = @_;
                my $animal = $class->new_instance('$.name' => $name);
                $class->meta()->add_animal($animal);
                return $animal;
            }
        }
    }
};

{
    can_ok('Eagle', 'make');
    ok(Eagle->isa('Eagle'), '... Eagle isa Eagle');
    ok(Eagle->isa('Bird'), '... Eagle isa Bird');
    ok(Eagle->is_kind_of('Species'), '... Eagle is kind of Species');

    can_ok('Eagle', 'meta');
    my $meta_eagle = Eagle->meta();
    isa_ok($meta_eagle, 'Eagle::Kind');
    isa_ok($meta_eagle, 'Species::Kind');
    isa_ok($meta_eagle, 'Perl6::Object::Kind');

    ok($meta_eagle->is_kind_of('Species'), '... $meta_eagle is a kind of species');

    ok(Eagle->is_extinct(), '... the Eagle is extinct');

    my $eagle = Eagle->make('Fred');
    ok($eagle->isa('Eagle'), '... $eagle isa Eagle');
    ok($eagle->isa('Bird'), '... $eagle isa Bird');

    ok(!Eagle->is_extinct(), '... the Eagle is no longer extinct');

    can_ok($eagle, 'name');
    can_ok($eagle, 'fly');

    is($eagle->name, 'Fred', '... got the right name');
    like($eagle->fly(), qr/^The \(Eagle\=HASH\(0x.*\)\) is Flying\!$/, '... the $eagle can fly()');

    # check some errors

    $@ = undef;
    eval { $eagle->make() };
    ok($@, '... $eagle cannot make()');

    $@ = undef;
    eval { $eagle->is_kind_of() };
    ok($@, '... $eagle cannot is_kind_of()');

    $@ = undef;
    eval { Eagle->fly() };
    ok($@, '... Eagle cannot fly()');

    $@ = undef;
    eval { Eagle->name() };
    ok($@, '... Eagle cannot name()');

    $@ = undef;
    eval { Eagle->is_instance_of() };
    ok($@, '... Eagle cannot is_instance_of()');

#    diag Dumper $eagle;
#    diag Dumper $meta_eagle;
}

1;