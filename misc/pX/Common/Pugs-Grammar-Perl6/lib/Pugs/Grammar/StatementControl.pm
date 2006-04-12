package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

*statement_list
    = Pugs::Compiler::Rule->compile( q(
    
            <Pugs::Grammar::Expression.parse> \;? :
            {
                #print "expression...\n";
                return { 
                    statements => [
                        $/{'Pugs::Grammar::Expression.parse'}->(),
                    ]
                }
            }
            
    |
    
            <Pugs::Grammar::StatementControl.parse> 
            {
                return { 
                    statements => [
                        $/{'Pugs::Grammar::StatementControl.parse'}->(),
                    ]
                }
            }

            
    ) )->code;

BEGIN {
    __PACKAGE__->add_rule(
        '{' => q(
            # { print "block...\n" }
            <Pugs::Grammar::StatementControl.statement_list> \}   # XXX
            {
                #print "block...\n";
                return { bare_block =>
                    $/{'Pugs::Grammar::StatementControl.statement_list'}->() ,}
            }
    ) );
    __PACKAGE__->add_rule(
        if =>  q(
            #{ print "if... $_[0]\n" }
            <Pugs::Grammar::Expression.parse>   
            #{ print "if expression parsed \n" }
            \{ <Pugs::Grammar::StatementControl.statement_list> \}
            { 
                # print "if parsed \n";
                return { 
                  if => {
                    exp   => $/{'Pugs::Grammar::Expression.parse'}->(),
                    block => $/{'Pugs::Grammar::StatementControl.statement_list'}->(),
                }
              } 
            }
    ) );
    __PACKAGE__->add_rule(
        while => q(
            <Pugs::Grammar::Expression.parse>   
            \{ <Pugs::Grammar::StatementControl.statement_list> \}
            { return { 
                while => {
                    exp   => $/{'Pugs::Grammar::Expression.parse'}->(),
                    block => $/{'Pugs::Grammar::StatementControl.statement_list'}->(),
                }
              } 
            }
    ) );
=for later
    __PACKAGE__->add_rule(
        q() => q(
            <Pugs::Grammar::Expression.parse> \;?
            {
                # print "expression...\n";
                return { statement => $/{'Pugs::Grammar::Expression.parse'}->() ,}
            }
    ) );
=cut
    __PACKAGE__->recompile;
}


1;
