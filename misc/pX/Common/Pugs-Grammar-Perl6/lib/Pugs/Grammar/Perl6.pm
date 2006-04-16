package Pugs::Grammar::Perl6;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::StatementControl;
use Pugs::Grammar::Expression;
use Pugs::Grammar::Pod;

# TODO - redefine <ws> to test Pod.pm after each \n

*parse = Pugs::Compiler::Rule->compile( q(
    
            <Pugs::Grammar::Expression.parse> 
            {
                return $/{'Pugs::Grammar::Expression.parse'}->()
            }
            
    ) )->code;

1;
