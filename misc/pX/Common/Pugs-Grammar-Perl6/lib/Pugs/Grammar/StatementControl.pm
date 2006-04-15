package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST
# TODO - redefine <ws> to test Pod.pm after each \n

#~ *statement_list
    #~ = Pugs::Compiler::Rule->compile( q(
    
            #~ #{ print "trying Grammar::StatementControl::parse $_[0] \n"; }
            #~ <?ws>?
            #~ <Pugs::Grammar::StatementControl.parse> 
            #~ #{ print "end of statement\n"; }
            #~ <?ws>?
            #~ <statement_list>?
            #~ {
                #~ #print "statement... $/ \n";
                #~ my $match = $/{'statement_list'}[0];
                #~ #print "match ", ref $match, "\n";
                #~ #print "match: ",Dumper $match->();
                #~ #my @a = @{ $match->() };
                #~ return [
                    #~ $/{'Pugs::Grammar::StatementControl.parse'}->(),
                    #~ @{ $match->() },
                #~ ];
            #~ }
        #~ |
            #~ #{ print "trying Grammar::Expression::parse $_[0] \n"; }
            #~ <Pugs::Grammar::Expression.parse> \;?
            #~ #\;
            #~ <statement_list>?
            #~ {
                #~ #print "tail ", ${$_[0]}->{tail}, "\n";
                #~ #print "expression... $/ \n";
                #~ my $match = $/{'statement_list'}[0];
                #~ #print "match ", ref $match, "\n";
                #~ #print "match: ",Dumper $match->();
                #~ return [
                    #~ $/{'Pugs::Grammar::Expression.parse'}->(),
                    #~ @{ $match->() },
                #~ ];
            #~ }
        #~ |
            #~ { 
                #~ #print "no match... $/ \n";
                #~ return [];
            #~ }
            
    #~ ) )->code;

BEGIN {
    __PACKAGE__->add_rule(
        '{' => q(
                #{ print "block...\n" }
                <Pugs::Grammar::Perl6.parse> 
            \}  
            
            {
                #print "block...\n";
                return { bare_block =>
                    $/{'Pugs::Grammar::Perl6.parse'}->() ,}
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
                <Pugs::Grammar::Perl6.parse> 
            \}
            { 
                # print "if parsed \n";
                return { 
                  if => {
                    exp   => $/{'Pugs::Grammar::Expression.parse'}->(),
                    block => $/{'Pugs::Grammar::Perl6.parse'}->(),
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
                <Pugs::Grammar::Perl6.parse> 
            \}
            { return { 
                while => {
                    exp   => $/{'Pugs::Grammar::Expression.parse'}->(),
                    block => $/{'Pugs::Grammar::Perl6.parse'}->(),
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
