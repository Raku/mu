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
    } );
    $statement->add_op( {
        fixity => 'infix', name => '*', assoc => 'left',
        precedence => 'tighter', other => '+',
    } );
    $statement->add_op( {
        fixity => 'postcircumfix', name => '[', name2 => ']', assoc => 'left',
        precedence => 'tighter', other => '+',
    } );
    $statement->add_op( {
        fixity => 'postcircumfix', name => '{', name2 => '}', assoc => 'left',
        precedence => 'equal', other => '[',
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
$Data::Dumper::Sortkeys = 1;
use Test::More qw(no_plan);

=for nothing
sub ::contains {
    my $tree = shift;
    my $data = shift;
    my $ref = ref $tree;
    if ( $ref eq 'ARRAY' ) {
        for ( @$tree ) {
            return 1 if contains( $_, $data );
    }}
    if ( $ref eq 'HASH' ) {
        return 1 if exists ${$tree}{$data};
        for ( keys %$tree ) {
            return 1 if contains( ${$tree}{$_}, $data );
    }}
    return 1 if defined $tree && $data eq $tree;
    return;
}
=cut

sub ::postfix {
    my $tree = shift;
    my $ref = ref $tree;
    my $s = "$tree";
    if ( $ref eq 'ARRAY' ) {
        $s = join( ' ', map { postfix( $_ ) } @$tree );
    }
    if ( $ref eq 'HASH' ) {
        return postfix( ${$tree}{term} ) if exists ${$tree}{term};
        $s = join( ' ', map { postfix( ${$tree}{$_} ) } 
                ( grep { $_ !~ /^(op|fix|list)/ }
                  sort keys %$tree
                ), 
                ( exists ${$tree}{list} ? 'list' : () ) 
            );
        if ( defined ${$tree}{op1} ) {
            no warnings qw(uninitialized);
            my $fixity = ${$tree}{fixity};
            $fixity = $1 if $fixity =~ /^(.*)_/;
            $s .= ' ' . $fixity . ':<' . ${$tree}{op1} . ${$tree}{op2} . '>';
        }
    }
    $s =~ s/\s+/ /g;
    $s =~ s/^\s+|\s+$//;
    return $s;
}

{
    my $match = $grammar->parse( '3+4+5' );
    #print Dumper $match->();
    ok( 
        postfix( $match->() ) eq '3 4 infix:<+> 5 infix:<+>' || 
        postfix( $match->() ) eq '3 4 5 infix:<+> infix:<+>', 
        'AST looks ok' 
    );
    #print postfix( $match->() );
}

{
    my $match = $grammar->parse( '3+4*5' );
    #print Dumper $match->();
    ok( 
        postfix( $match->() ) eq '3 4 5 infix:<*> infix:<+>', 
        'AST +/* looks ok' 
    );
    #print postfix( $match->() );
}

{
    my $match = $grammar->parse( '3*4+5' );
    #print Dumper $match->();
    ok( 
        postfix( $match->() ) eq '3 4 infix:<*> 5 infix:<+>' 
        # || 
        # postfix( $match->() ) eq '3 4 5 infix:<+> infix:<*>'
        , 
        'AST +/* looks ok' 
    );
    #print postfix( $match->() );
    #print Dumper $match->();
}
__END__
{
    my $match = $grammar->parse( '{0+1+$a[7][8]{9}{10}[11]}{3};4+5' );
    print Dumper $match->();
    ok( contains( $match->(), '$a', 'AST looks ok' ) );
    #print postfix( $match->() );
}
__END__
{
    my $match = $grammar->parse( '{0+1+$a[7][8]}{3};4+5' );
    print Dumper $match->();
}
