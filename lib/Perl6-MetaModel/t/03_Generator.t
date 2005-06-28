#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use MetaModel;

=pod

This test file also demonstrates Kind's ability to abstract Design Patterns.
This demonstrates the following:

=over 4

=item Parameterized Class instance construction based values in a Kind level generator

=item Class attributes will generate rw accessors for public attributes

=back

=cut

class Generator => {
    kind => {
        attrs => [ '$:generator' ],
        init => sub {
            my ($self) = @_;
            $self->set_value('$:generator' => sub { die "No generator set" });
        },
        methods => {
            new => sub {
                my ($class) = @_;
                $class->new_instance($class->meta->generate())
            },
            set_generator => sub {
                my ($class, $generator) = @_;
                $class->meta->set_value('$:generator' => $generator);
            },
            generate => sub {
                my ($class) = @_;
                $class->meta->get_value('$:generator')->();
            }
        }
    }
};

class EvenNums => {
    kind_of => 'Generator',
    class => {
        attrs => [ '$.number' ],
    }
};

class OddNums => {
    kind_of => 'Generator',
    class => {
        attrs => [ '$.number' ],
    }
};

eval { EvenNums->new() };
ok($@, '... got an exception');
like($@, qr/^No generator set/, '... got the expected exception');

{
    my $tracker = 0;
    EvenNums->set_generator(sub { ('$.number' => $tracker += 2) });
}

eval { OddNums->new() };
ok($@, '... got an exception');
like($@, qr/^No generator set/, '... got the expected exception');

{
    my $tracker = -1;
    OddNums->set_generator(sub { ('$.number' => $tracker += 2) });
}

foreach my $test_num (1 .. 10) {
    if ($test_num % 2 == 0) {
        my $num = eval { EvenNums->new() };
        ok(!$@ && defined $num, '... no error occured creating EvenNums instance');
        ok($num->isa('EvenNums'), '... we got an EvenNums object');
        is($num->number, $test_num, '... and it has the right number in it');
    }
    else {
        my $num = eval { OddNums->new() };
        ok(!$@ && defined $num, '... no error occured creating OddNums instance');
        ok($num->isa('OddNums'), '... we got the right OddNums object');
        is($num->number, $test_num, '... and it has the right number in it');
    }
}

