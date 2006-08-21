package Pugs::Grammar::Infix;
use strict;
use warnings;
use Pugs::Grammar::Operator;
use base qw(Pugs::Grammar::BaseCategory);

sub add_rule {
    # print "add infix operator\n";
    my $self = shift;
    my %opt = @_;
    #print "Infix add: @{[ %opt ]} \n";

    Pugs::Grammar::Operator::add_rule( $self, %opt,
        fixity => 'infix', 
    );
    Pugs::Grammar::Operator::add_rule( $self, %opt,
        precedence => 'equal',
        other  => $opt{name},
        fixity => 'infix', 
        name   => 'infix:<' . $opt{name} . '>',
    );
    $self->SUPER::add_rule( 
        $opt{name}, 
        '{ return { op => "' . $opt{name} . '" ,} }' );
    $self->SUPER::add_rule( 
        "infix:<" . $opt{name} . ">",
        '{ return { op => "infix:<' . $opt{name} . '>" ,} }' );
}

sub add_same_precedence_ops {
    my ($class, $opt, $other, @ops) = @_;
    $class->add_rule(other => $other, precedence => 'equal', name => $_, %$opt) for @ops;
}

BEGIN {
    __PACKAGE__->add_rule( 
        name => '*',
        assoc => 'left',
    );
    __PACKAGE__->add_same_precedence_ops( { assoc => 'left'}, qw(* / % x xx +& +< +> ~& ~< ~> ?& ) );

    __PACKAGE__->add_rule(
        name => '**',
        assoc => 'right',
        precedence => 'tighter',
        other => 'infix:<*>',
    );
    
    __PACKAGE__->add_rule( 
        name => '+',
        assoc => 'left',
        precedence => 'looser',
        other => '*',
    );

    __PACKAGE__->add_same_precedence_ops( { assoc => 'left'}, qw(+ - ~ +| +^ ~| ~^ ?| ?^) );

    __PACKAGE__->add_rule( 
        name => '..',
        assoc => 'left',
        precedence => 'looser',
        other => '+',
    );

    __PACKAGE__->add_same_precedence_ops( { assoc => 'left'},
                                   qw(.. but does <=> leg cmp ^.. ..^
                                   ^..^ ff ^ff ff^ ^ff^ fff ^fff fff^
                                   ^fff^) );

    __PACKAGE__->add_rule( 
        name => '|',
        assoc => 'left',
        precedence => 'looser',
        other => '..',
    );

    __PACKAGE__->add_rule( 
        name => 'eq',
        assoc => 'chain',
        precedence => 'looser',
        other => 'infix:<|>',
    );

    __PACKAGE__->add_same_precedence_ops( { assoc => 'chain'}, qw(eq !=
                                   == < <= > >= ne lt le gt ge
                                   === eqv !== !~~ !eq !=== !eqv)
                                   );
    __PACKAGE__->add_same_precedence_ops( { assoc => 'left'}, qw(eq ~~ =:=  !=:=) );

    __PACKAGE__->add_rule( 
        name => '&&',
        assoc => 'right',
        precedence => 'looser',
        other => 'eq',
    );
    
    __PACKAGE__->add_rule( 
        name => '||',
        assoc => 'right',
        precedence => 'looser',
        other => '&&',
    );

    __PACKAGE__->add_same_precedence_ops({ assoc => 'right'}, qw(|| //) );

    __PACKAGE__->add_rule( 
        name => '=',
        assoc => 'right',
        precedence => 'looser',
        other => '||',
    );
    __PACKAGE__->add_same_precedence_ops({ assoc => 'right'}, qw(= := ::= => += -= **= xx= x= .= ~= =~) );


    __PACKAGE__->add_rule( 
        name => 'Y',
        assoc => 'list',
        precedence => 'looser',
        other => '=',
    );
    __PACKAGE__->add_rule( 
        name => '¥',
        assoc => 'list',
        precedence => 'equal',
        other => 'Y',
    );
    __PACKAGE__->add_rule( 
        name => ',',
        assoc => 'list',
        precedence => 'equal',
        other => 'Y',
    );
    __PACKAGE__->add_rule( 
        name => ';',
        assoc => 'list',
        precedence => 'equal',
        other => 'Y',
    );
    
    __PACKAGE__->add_rule( 
        name => 'and',
        assoc => 'left',
        precedence => 'looser',
        other => 'Y',
    );
    
    __PACKAGE__->add_rule( 
        name => 'or',
        assoc => 'left',
        precedence => 'looser',
        other => 'and',
    );
    __PACKAGE__->add_rule( 
        name => 'err',
        assoc => 'left',
        precedence => 'equal',
        other => 'or',
    );
    __PACKAGE__->add_rule( 
        name => 'xor',
        assoc => 'left',
        precedence => 'equal',
        other => 'or',
    );

    __PACKAGE__->recompile;
}

1;
