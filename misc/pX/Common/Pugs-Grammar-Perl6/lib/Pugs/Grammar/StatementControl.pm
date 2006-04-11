package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

BEGIN {
    __PACKAGE__->add_rule( 
        '{' => q( 
            <Pugs::Grammar::Expression.parse> \}   # XXX
            { return { bare_block => $/{'Pugs::Grammar::Expression.parse'}->() ,} } 
    ) );
    __PACKAGE__->add_rule( 
        if =>  q( 
            <Pugs::Grammar::Expression.parse>    # XXX
            \{ <Pugs::Grammar::Expression.parse> \} 
            { return { if => $/{'Pugs::Grammar::Expression.parse'}->() ,} } 
    ) );
    __PACKAGE__->add_rule( 
        while => q( 
            <Pugs::Grammar::Expression.parse>    # XXX
            \{ <Pugs::Grammar::Expression.parse> \} 
            { return { while => $/{'Pugs::Grammar::Expression.parse'}->() ,} } 
    ) );
    __PACKAGE__->add_rule( 
        q() => q( 
              <Pugs::Grammar::Expression.parse> 
            | <Pugs::Grammar::Expression.parse> \;
            { return { statement => $/{'Pugs::Grammar::Expression.parse'}->() ,} } 
    ) );
    __PACKAGE__->recompile;
}


1;
