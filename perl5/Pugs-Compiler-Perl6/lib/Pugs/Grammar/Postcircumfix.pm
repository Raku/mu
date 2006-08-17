package Pugs::Grammar::Postcircumfix;
use strict;
use warnings;
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

use Pugs::Grammar::Infix;

# XXX - this module is currently unused
# XXX - see Pugs::Grammar::Term, Pugs::Grammar::Quote

sub add_rule {
    my $self = shift;
    my %opt = @_;
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'postcircumfix', 
        assoc => 'non',
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        precedence => 'equal',
        other  => $opt{name},
        fixity => 'postcircumfix', 
        assoc => 'non',
        name => 'postcircumfix:<' . $opt{name} . ' ' . $opt{name2} . '>',
    );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $opt{name} . '" ,} }' );
    $self->SUPER::add_rule( 
        $opt{name2}, 
        '{ return { op => "' . $opt{name2} . '" ,} }' );
    $self->SUPER::add_rule( 
        "postcircumfix:<" . $opt{name} . " " . $opt{name2} . ">",
        '{ return { op => "postcircumfix:<' . $opt{name} . ' ' . $opt{name2} . '>" ,} }' );
}


BEGIN {

    __PACKAGE__->recompile;
}

1;
