package Pugs::Grammar::Operator;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Precedence;

our $operator = Pugs::Grammar::Precedence->new( 
        grammar => 'Pugs::Grammar::Operator',
);

sub add_rule {
    my $self = shift;
    my %opt = @_;

    $self->SUPER::add_rule( $opt{name} => $opt{rule} );
    delete $opt{rule};
    $operator->add_op( \%opt );
}

1;
