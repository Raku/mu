package Pugs::Emitter::Perl6::Perl5::node;

use strict;
use warnings;

sub root {
    'Pugs::Emitter::Perl6::Perl5::'
}

sub node {
    ( $_[0]->root . $_[1] )->new( { name => $_[2] } );
}

sub new {
    my $self = $_[1];  # { name => '$scalar5' }
    bless $self, $_[0];
    return $self;
}

1;
