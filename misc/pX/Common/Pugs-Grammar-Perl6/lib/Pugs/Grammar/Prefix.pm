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
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'prefix', 
        name => 'prefix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( $opt{name} => $opt{rule} );
}


BEGIN {
    __PACKAGE__->add_rule( 
        name => '+',
        assoc => 'left',
        precedence => 'tighter',
        other => '*',
        rule => '{ return { op => "prefix:<+>" ,} }' );
    __PACKAGE__->add_rule(
        name => '-',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
        rule => '{ return { op => "prefix:<->" ,} }' );
    __PACKAGE__->recompile;
}

1;
