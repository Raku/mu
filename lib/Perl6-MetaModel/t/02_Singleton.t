#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use MetaModel;

=pod

This test file demonstrates how Kind's can be used to abstract 
Design Patterns, in particular a Singleton. It demonstrates the
following behaviors:

=over 4

=item Kind as a controlled Class instance factory

=item Kind's behavior is not visibile to the Class instance

=item Kind's behavior is need not be known by the Class instance

=item Class attributes will generate rw accessors for public attributes

=back

=cut

class Singleton => {
    kind => {
        attr => [ '$:instance' ],
        methods => {
            instance => sub {
                my ($class) = @_;
                $class->meta->set_value('$:instance' => $class->new_instance())
                    unless $class->meta->get_value('$:instance');
                $class->meta->get_value('$:instance');
            }
        }
    }
};


class Service => {
    kind_of => 'Singleton',
    class => {
        attrs => [ '@.test' ],
        init => sub {
            my ($self) = @_;
            $self->set_value('@.test' => [ 1, 2, 3 ]);
        }
    }
};

my $service = Service->instance();
ok($service->isa('Service'), '... it is a Service instance');

my $service2 = Service->instance();
ok($service2->isa('Service'), '... it is a Service instance');

is($service, $service2, '... they two are the same instance');

my $service3 = Service->instance();
ok($service3->isa('Service'), '... it is a Service instance');

is($service, $service3, '... they two are the same instance');
is($service2, $service3, '... they two are the same instance');

is($service->test, $service2->test, '... even the instance values are the same');
is($service->test, $service3->test, '... even the instance values are the same');
is($service2->test, $service3->test, '... even the instance values are the same');

ok(!$service->can('instance'), '... instance() is a class method');
ok(!$service2->can('instance'), '... instance() is a class method');
ok(!$service3->can('instance'), '... instance() is a class method');
