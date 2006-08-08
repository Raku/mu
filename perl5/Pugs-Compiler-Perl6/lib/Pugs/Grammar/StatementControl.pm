package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

BEGIN {
    __PACKAGE__->add_rule(
        'for' =>  q( 
            # { print "statement for \n"; }
            <?ws> 
            $<exp1> := <Pugs::Grammar::Perl6.perl6_expression('no_blocks',0)> <?ws>?
            $<exp2> := <Pugs::Grammar::Perl6.block>        
            { return { 
                    statement => 'for',
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'while' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Perl6.perl6_expression('no_blocks',0)> <?ws>?
            $<exp2> := <Pugs::Grammar::Perl6.block>        
            { return { 
                    statement => 'while',
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'until' =>  q( 
            <?ws> 
            $<exp1> := <Pugs::Grammar::Perl6.perl6_expression('no_blocks',0)> <?ws>?
            $<exp2> := <Pugs::Grammar::Perl6.block>        
            { return { 
                    statement => 'until',
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
            } }
        ) );
    __PACKAGE__->add_rule(
        'loop' =>  q( 
            <?ws> 
            [
              <'('> 
                $<exp1> := <Pugs::Grammar::Perl6.perl6_expression_or_null> <?ws>? <';'>
                $<exp2> := <Pugs::Grammar::Perl6.perl6_expression_or_null> <?ws>? <';'>
                $<exp3> := <Pugs::Grammar::Perl6.perl6_expression_or_null> <?ws>? 
              <')'> 
              <?ws>? $<content> := <Pugs::Grammar::Perl6.block>
              { return { statement => 'loop',
                     exp1      => $_[0]{exp1}->(),
                     exp2      => $_[0]{exp2}->(),
                     exp3      => $_[0]{exp3}->(),
                     content   => $_[0]{content}->() }
              }
            |
              <Pugs::Grammar::Perl6.block> <?ws>?
              { return { statement => 'loop',
                     content   => $_[0]{'Pugs::Grammar::Perl6.block'}->() }
              }
            |
              # XXX better error messages
              { return { die "invalid loop syntax" } }
           ]
        ) );
    __PACKAGE__->recompile;
}


1;
