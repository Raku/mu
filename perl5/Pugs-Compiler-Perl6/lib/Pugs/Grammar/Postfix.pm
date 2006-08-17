package Pugs::Grammar::Postfix;
use strict;
use warnings;
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

use Pugs::Grammar::Infix;

sub add_rule {
    my $self = shift;
    my %opt = @_;
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'postfix', 
        assoc => 'non',
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        precedence => 'equal',
        other  => $opt{name},
        fixity => 'postfix', 
        assoc => 'non',
        name => 'postfix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $opt{name} . '" ,} }' );
    $self->SUPER::add_rule( 
        "postfix:<" . $opt{name} . ">",
        '{ return { op => "postfix:<' . $opt{name} . '>" ,} }' );
}


BEGIN {
    __PACKAGE__->add_rule( 
        name => '++',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<++>',
    );
    __PACKAGE__->add_rule(
        name => '--',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<++>',
    );
    __PACKAGE__->add_rule(
        name => '?',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<++>',
    );

    __PACKAGE__->recompile;
}

1;
