package Pugs::Grammar::StatementModifier;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

BEGIN {

    __PACKAGE__->add_rule(
        'if' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Expression.parse> 
            { return { 
                statement => 'if',
                exp1 => $_[0]{exp1}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'unless' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Expression.parse> 
            { return { 
                statement => 'unless',
                exp1 => $_[0]{exp1}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'for' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Expression.parse> 
            { return { 
                statement => 'for',
                exp1 => $_[0]{exp1}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'while' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Expression.parse> 
            { return { 
                statement => 'while',
                exp1 => $_[0]{exp1}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'until' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Expression.parse> 
            { return { 
                statement => 'until',
                exp1 => $_[0]{exp1}->(),
            } }
        ) );

    __PACKAGE__->recompile;
}


1;
