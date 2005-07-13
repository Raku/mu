#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;

use Perl6::MetaModel;

=pod

This test file is testing the SUPER call in Roles.

=cut

role rSyncReader => {
    methods => {
        read => sub {
            my $self = shift;
            'LOCK -> ' . $self->SUPER::read() . ' -> UNLOCK'
        }
    }
};

class Reader => {
    instance => {
        methods => {
            read => sub { 'Reader::read' }
        }
    }
};

class SyncReader => {
    is => [ 'Reader' ],
    does => [ 'rSyncReader' ],
};

my $sync_reader = SyncReader->new();
isa_ok($sync_reader, 'SyncReader');
isa_ok($sync_reader, 'Reader');

ok($sync_reader->does('rSyncReader'), '... $sync_reader does rSyncReader');

can_ok($sync_reader, 'read');
is($sync_reader->read(), 'LOCK -> Reader::read -> UNLOCK', '... got the right output');

