package Pugs::Grammar::Prefix;
use strict;
use warnings;
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::Infix;

sub add_rule {
    my $self = shift;
    my %opt = @_;
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        fixity => 'prefix', 
        assoc => 'non',
    );
    $self->Pugs::Grammar::Operator::add_rule( %opt,
        precedence => 'equal',
        other  => $opt{name},
        fixity => 'prefix', 
        assoc => 'non',
        name => 'prefix:<' . $opt{name} . '>',
    );
    my $name = quotemeta( $opt{name} );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $name . '" ,} }' );
    $self->SUPER::add_rule( 
        "prefix:<" . $opt{name} . ">",
        '{ return { op => "prefix:<' . $name . '>" ,} }' );
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
    __PACKAGE__->add_rule(
        name => '?',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '~',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '@',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '%',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '$',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '&',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '!',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '*',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => ':',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule(
        name => '=',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<+>',
    );
    __PACKAGE__->add_rule( 
        name => '++',
        assoc => 'left',
        precedence => 'tighter',
        other => '*',
    );
    __PACKAGE__->add_rule(
        name => '--',
        assoc => 'left',
        precedence => 'equal',
        other => 'prefix:<++>',
    );

    # experimental
    __PACKAGE__->add_rule(
        name => 'do',
        assoc => 'non',
        precedence => 'equal',
        other => 'infix:<+>',
    );
    __PACKAGE__->add_rule(
        name => 'try',
        assoc => 'non',
        precedence => 'equal',
        other => 'infix:<+>',
    );

    __PACKAGE__->recompile;
}

1;
