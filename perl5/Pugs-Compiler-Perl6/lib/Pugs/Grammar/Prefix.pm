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
        precedence => 'equal',
        other  => $opt{name},
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
    
    #__PACKAGE__->add_rule(
    #    name => 'say',
    #    assoc => 'left',
    #    precedence => 'looser',
    #    other => 'infix:<,>',
    #);
    #__PACKAGE__->add_rule(
    #    name => 'substr',
    #    assoc => 'left',
    #    precedence => 'equal',
    #    other => 'prefix:<say>',
    #);
    
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

    #for ( qw( print use push pop ) ) {
    #    __PACKAGE__->add_rule(
    #        name => $_,
    #        assoc => 'left',
    #        precedence => 'equal',
    #        other => 'say',
    #    );
    #}
    for ( qw( my our has ) ) {
        __PACKAGE__->add_rule(
            name => $_,
            assoc => 'left',
            precedence => 'tighter',
            other => '=',
        );
    }

    __PACKAGE__->recompile;
}

1;
