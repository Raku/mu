# pX/Common/iterator_engine_pge_P6Rule.grammar.pl - fglock
#
# experimental implementation of P6Rule.grammar in 'iterator_engine'
#
# see: http://svn.perl.org/parrot/trunk/compilers/pge/P6Rule.grammar


use strict;
use warnings;

require 'iterator_engine.pl';

{
  # grammar PGE::P6Rule;
  package grammar2;
  no warnings 'once';

  # rule pattern { <flag>* <alternation> }
  *pattern = 
    ruleop::concat( 
      ruleop::greedy_star( 
        \&flag 
      ), 
      \&alternation 
    );

  # rule flag { \:<ident> [ \( <code> \) | \[ <code> \] ]? }
  *flag = 
    ruleop::concat( 
      ruleop::concat( 
        ruleop::constant( ':' ),
        \&ident
      ),
      ruleop::optional(
        ruleop::concat( 
          ruleop::concat( 
            ruleop::constant( '(' ),
            \&code
          ),
          ruleop::constant( ')' ),
        ),
        ruleop::concat( 
          ruleop::concat( 
            ruleop::constant( '[' ),
            \&code
          ),
          ruleop::constant( ']' ),
        ),
      )
    );

    # TODO

    sub code { print "<code> not implemented\n" };
    sub ident { print "<ident> not implemented\n" };
}

