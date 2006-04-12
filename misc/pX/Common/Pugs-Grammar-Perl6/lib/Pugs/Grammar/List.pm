package Pugs::Grammar::List;
use strict;
use warnings;
#use base qw(Pugs::Grammar::Operator);
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

sub add_rule {
    # print "add infix operator\n";
    my $self = shift;
    my %opt = @_;
    #print "Infix add: @{[ %opt ]} \n";

    Pugs::Grammar::Operator::add_rule( $self, %opt,
        fixity => 'infix', 
        assoc => 'list',
    );
    Pugs::Grammar::Operator::add_rule( $self, %opt,
        fixity => 'infix', 
        assoc => 'list',
        name => 'infix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $opt{name} . '" ,} }' );
    $self->SUPER::add_rule( 
        "infix:<' . $opt{name} . '>",
        '{ return { op => "infix:<' . $opt{name} . '>" ,} }' );
}

BEGIN {
    __PACKAGE__->add_rule( 
        name => 'Y',
        precedence => 'looser',
        other => '+',
    );
    __PACKAGE__->add_rule( 
        name => '¥',
        precedence => 'equal',
        other => 'Y',
    );
    __PACKAGE__->add_rule( 
        name => ',',
        precedence => 'equal',
        other => 'Y',
    );
    __PACKAGE__->add_rule( 
        name => ';',
        precedence => 'looser',
        other => 'Y',
    );
    __PACKAGE__->recompile;
}

1;
