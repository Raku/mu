package Pugs::Grammar::Prefix;
use strict;
use warnings;
#use base qw(Pugs::Grammar::Operator);
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

use Pugs::Grammar::Infix;

# TODO - generate AST
# TODO - prefix:{'+'}
# TODO - ~ ? 

sub add_rule {
    my $self = shift;
    my %opt = @_;
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'prefix', 
        assoc => 'non',
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'prefix', 
        assoc => 'non',
        name => 'prefix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $opt{name} . '" ,} }' );
    $self->SUPER::add_rule( 
        "prefix:<" . $opt{name} . ">",
        '{ return { op => "prefix:<' . $opt{name} . '>" ,} }' );
}


BEGIN {
    __PACKAGE__->add_rule( 
        name => '+',
        assoc => 'left',
        precedence => 'tighter',
        other => '*',
    );
    __PACKAGE__->add_rule(
        name => '-',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->recompile;
}

1;
