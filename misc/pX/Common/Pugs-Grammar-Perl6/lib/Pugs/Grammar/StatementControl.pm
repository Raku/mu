package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

*statement_list
    = Pugs::Compiler::Rule->compile( q(
    
            #{ print "trying Grammar::StatementControl::parse $_[0] \n"; }
            <?ws>?
            <Pugs::Grammar::StatementControl.parse> 
            #{ print "end of statement\n"; }
            <?ws>?
            {
                #print "statement... $/ \n";
                return $/{'Pugs::Grammar::StatementControl.parse'}->();
            }
        |
            #{ print "trying Grammar::Expression::parse $_[0] \n"; }
            <Pugs::Grammar::Expression.parse> \;? :
            {
                #print "expression... $/ \n";
                return $/{'Pugs::Grammar::Expression.parse'}->();
            }
        |
            { 
                #print "no match... $/ \n";
                return {};
            }
            
    ) )->code;

BEGIN {
    __PACKAGE__->add_rule(
        '{' => q(
                #{ print "block...\n" }
                <Pugs::Grammar::StatementControl.statement_list> 
            \}  
            
            {
                #print "block...\n";
                return { bare_block =>
                    $/{'Pugs::Grammar::StatementControl.statement_list'}->() ,}
            }
    ) );
    __PACKAGE__->add_rule(
        if =>  q(
            #{ print "if... $_[0]\n" }
            <?ws>
            <Pugs::Grammar::Expression.parse>   
            <?ws>?
            #{ print "if expression parsed \n" }
            \{ 
                <Pugs::Grammar::StatementControl.statement_list> 
            \}
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
            <?ws>
            <Pugs::Grammar::Expression.parse>   
            <?ws>?
            \{ 
                <Pugs::Grammar::StatementControl.statement_list> 
            \}
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
