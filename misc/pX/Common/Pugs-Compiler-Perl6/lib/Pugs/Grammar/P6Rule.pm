package  Pugs::Grammar::P6Rule;
use strict;
use warnings;

use base qw(Pugs::Grammar::Rule);
use Pugs::Runtime::Match; # overload doesn't work without this ???

*parsed_code = Pugs::Compiler::Token->compile( q(
    <Pugs::Grammar::Perl6.parse>
    { 
        #print "bootstrapped\n";
        return { bare_block => $/{'Pugs::Grammar::Perl6.parse'}() },
    }
))->code;
    
1;

