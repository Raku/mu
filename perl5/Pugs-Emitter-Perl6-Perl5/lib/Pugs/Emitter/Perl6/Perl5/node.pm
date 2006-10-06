package Pugs::Emitter::Perl6::Perl5::node;

use strict;
use warnings;
use Pugs::Runtime::Common;

our $id = 'n' . int( 1000 + rand(9000) );
sub new_id { $id++ }

sub root {
    'Pugs::Emitter::Perl6::Perl5::'
}

sub node {
    ( $_[0]->root . $_[1] )->new( { name => $_[2] } );
}

sub ::unicode_sub($&) {
    my $name = (caller)[0] . '::' . Pugs::Runtime::Common::mangle_ident($_[0]);
    #print "Name: $name \n";
    no strict 'refs';
    *{$name} = $_[1]; 
}

sub new {
    my $self = $_[1];  # { name => '$scalar5' }
    bless $self, $_[0];
    return $self;
}

1;
