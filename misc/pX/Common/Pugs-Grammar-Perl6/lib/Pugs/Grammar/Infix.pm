package Pugs::Grammar::Infix;
use strict;
use warnings;
use base qw(Pugs::Grammar::Operator);

sub add_rule {
    my $self = shift;
    my %opt = @_;
    $self->SUPER::add_rule( %opt,
        fixity => 'infix', 
    );
    $self->SUPER::add_rule( %opt,
        fixity => 'infix', 
        name => 'infix:<' . $opt{name} . '>',
    );
}

BEGIN {
    __PACKAGE__->add_rule( 
        name => '+',
        assoc => 'left',
        rule => '{ return { op => "infix:<+>" ,} }' );
    __PACKAGE__->add_rule( 
        name => '-',
        assoc => 'left',
        precedence => 'equal',
        other => '+',
        rule => '{ return { op => "infix:<->" ,} }' );
    __PACKAGE__->add_rule( 
        name => '*',
        assoc => 'left',
        precedence => 'tighter',
        other => '+',
        rule => '{ return { op => "infix:<*>" ,} }' );
    __PACKAGE__->add_rule( 
        name => '/',
        assoc => 'left',
        precedence => 'equal',
        other => '*',
        rule => '{ return { op => "infix:</>" ,} }' );
    __PACKAGE__->recompile;
}

1;
