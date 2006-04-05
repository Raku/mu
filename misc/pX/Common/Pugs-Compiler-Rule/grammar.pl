my $rule = 'Pugs::Compiler::Rule';
my $grammar = 'Pugs::Grammar::Perl6';

package Pugs::Grammar::Perl6;

use Pugs::Compiler::Rule;
use base Pugs::Grammar::Base;

*parse = $rule->compile( q(
    <header> <body>*
    { 
        return { 
            program_header => $<header>(), 
            program => [ map { $_->() } @{$<body>} ],
        } 
    }
) )->code;

# things that are only valid in the start of a file
# - shebang, 
# - version info 
# - module, class, grammar extending to the end of file 
*header = $rule->compile( q(
    { return [] }
) )->code;

# main program
*body = $rule->compile( q(
        [ \;      
            { return { null_statement => 0 ,} } 
        ] |
        [ <bare_statement> 
            { return $<bare_statement>() }
        ] | 
        [ <statement> 
            [ <before  \;  |  \}  >  |  
              $ 
            ] 
            { return $<statement>() }
        ]
) )->code;

# category 'bare_statement'
# statements that don't need a semicolon
# - bare block
# - if, while, for
# - subroutine, method, coro definitions
# - module, class, grammar

# category 'statement'
# statements that need a \; or \} or $$ terminator 
# - subroutine calls
# - expressions, my, our

# ---------

*bare_statement = $rule->compile( q(
    \{ <body>* \}
    { 
        return { 
            block => [ map { $_->() } @{$<body>} ],
        } 
    }
) )->code;

*statement = $rule->compile( q(
    (<statement.parse>)
    { return { expr => $/[0]() ,} }
) )->code;

# ----------

    use Pugs::Grammar::Category;
    my $statement = Pugs::Grammar::Category->new( {
        name => 'statement',
        operand => 'term',
    } );
    $statement->add_op( {
        fixity => 'infix', name => '+', assoc => 'left',
        precedence => 'looser', other => '*',
    } );
    $statement->add_op( {
        fixity => 'postcircumfix', name => '[', name2 => ']', assoc => 'left',
        precedence => 'tighter', other => '+',
    } );

    package statement;
    use Pugs::Grammar::Base;
    use Data::Dumper;
    no warnings qw( once );
    *term = Pugs::Compiler::Rule->compile( q( 
        [ (\d+) { return {num=>$() ,} } ] |
        [ ( [ \$ | \@ | \% | \& ] \w+) { return {name=>$(),} } ]  
    ) )->code;
    eval $statement->emit_grammar_perl5();
    #print "statement grammar: ", $statement->emit_grammar_perl5();

# ------------

package main;

use strict;
use warnings;
use Pugs::AST::Expression;
use Data::Dumper;
{
    my $match = $grammar->parse( '{0+1+$a[7][8]}{3};4+5' );
    print Dumper $match->();
}
