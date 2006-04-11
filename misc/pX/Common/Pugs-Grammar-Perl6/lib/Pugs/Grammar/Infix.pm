package Pugs::Grammar::Infix;
use strict;
use warnings;
#use base qw(Pugs::Grammar::Operator);
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

sub add_rule {
    print "add infix operator\n";
    my $self = shift;
    my %opt = @_;
    print "Infix add: @{[ %opt ]} \n";

    Pugs::Grammar::Operator::add_rule( $self, %opt,
        fixity => 'infix', 
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'infix', 
        name => 'infix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( $opt{name} => $opt{rule} );
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
