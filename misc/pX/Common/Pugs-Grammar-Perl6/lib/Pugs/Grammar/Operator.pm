package Pugs::Grammar::Operator;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Precedence;

our $operator;

BEGIN {
    $operator = Pugs::Grammar::Precedence->new( 
        grammar => 'Pugs::Grammar::Operator',
    );
    print "created operator table\n";
}

sub add_rule {
    print "add operator\n";
    my $self = shift;
    my %opt = @_;
    print "Operator add: @{[ %opt ]} \n";

    delete $opt{rule};
    $operator->add_op( \%opt );
}

use Pugs::Grammar::Infix;
use Pugs::Grammar::Prefix;

# use Pugs::Runtime::Match;

# TODO - implement the "magic hash" dispatcher

our %hash;

sub recompile {
    my $class = shift;
    %hash = (
        %Pugs::Grammar::Infix::hash,
        %Pugs::Grammar::Prefix::hash,
    );
    $class->SUPER::recompile;
}

BEGIN {
    __PACKAGE__->recompile;
}

1;
