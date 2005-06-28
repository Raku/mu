#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use MetaModel;

=pod

This test file is testing the SUPER call in Roles.

=cut

role rSyncReader => {
    methods => {
        read => sub {
            my $self = shift;
            'LOCK -> ' . $self->SUPER('read') . ' -> UNLOCK'
        }
    }
};

class Reader => {
    class => {
        methods => {
            read => sub { 'Reader::read' }
        }
    }
};

class SyncReader => {
    extends => 'Reader',
    does => [ 'rSyncReader' ],
};

my $sync_reader = SyncReader->new_instance();
isa_ok($sync_reader, 'SyncReader');
isa_ok($sync_reader, 'Reader');

ok($sync_reader->does('rSyncReader'), '... $sync_reader does rSyncReader');

can_ok($sync_reader, 'read');
is($sync_reader->read(), 'LOCK -> Reader::read -> UNLOCK', '... got the right output');

