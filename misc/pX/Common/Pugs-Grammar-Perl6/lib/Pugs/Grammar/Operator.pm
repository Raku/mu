package Pugs::Grammar::Operator;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Precedence;

use Pugs::Grammar::Infix;
use Pugs::Grammar::Prefix;

our $operator = Pugs::Grammar::Precedence->new( 
        grammar => 'Pugs::Grammar::Operator',
);

sub add_rule {
print "add\n";
    my $self = shift;
    my %opt = @_;
    print "Operator add: @{[ %opt ]} \n";

    $self->SUPER::add_rule( $opt{name} => $opt{rule} );
    delete $opt{rule};
    $operator->add_op( \%opt );
}

1;
