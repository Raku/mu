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

package statement;
use Pugs::Grammar::Base;
use Pugs::Grammar::Category;
use Data::Dumper;
no warnings qw( once );

our $statement = Pugs::Grammar::Category->new( {
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
    precedence => 'tighter', other => '*',
} );
$statement->add_op( {
    fixity => 'postcircumfix', name => '{', name2 => '}', assoc => 'left',
    precedence => 'equal', other => '[',
} );
$statement->add_op( {
    fixity => 'circumfix', name => '(', name2 => ')', assoc => 'left',
    precedence => 'tighter', other => '[',
} );

*term = Pugs::Compiler::Rule->compile( q( 
      [ (\d+)                        { return { num  => $()     ,} } ] 
    | [ ( [ \$ | \@ | \% | \& ] \w+) { return { name => $()     ,} } ] 
    # | [ \( (<statement.parse>) \)    { return { expr => $/[0]() ,} } ]
) )->code;
eval $statement->emit_grammar_perl5();
#print "statement grammar: ", $statement->emit_grammar_perl6();

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

{
    print "# TODO - ws not allowed before subscript\n";
    my $match = $grammar->parse( '3*@a{$b}[$c]+5' );
    #print Dumper $match->();
    ok( 
        postfix( $match->() ) eq 
        '3 @a $b postcircumfix:<{}> $c postcircumfix:<[]> infix:<*> 5 infix:<+>', 
        'AST +/*/[]/{} looks ok' 
    );
    #print postfix( $match->() );
    #print Dumper $match->();
}

{
    my $match = $grammar->parse( '{1+@a{8}}' );
    ok( 
        postfix( $match->() ) eq 
        '1 @a 8 postcircumfix:<{}> infix:<+>', 
        'statement/expression inside a block' 
    );
    #print postfix( $match->() );
}

{
    # define a new operator globally
    {
        package statement;
        $statement->add_op( {
            fixity => 'prefix', name => '+', 
            precedence => 'looser', other => '[',
        } );
        no warnings qw( redefine );
        eval $statement->emit_grammar_perl5();
    }
    
    my $match = $grammar->parse( '+1+2' );
    ok( 
        postfix( $match->() ) eq 
        '1 prefix:<+> 2 infix:<+>', 
        'define a new operator globally; prefix+infix' 
    );
    #print postfix( $match->() );
}

{
    my $match = $grammar->parse( '3*(4+5)' );
    #print Dumper $match->();
    ok( 
        postfix( $match->() ) eq '3 4 5 infix:<+> circumfix:<()> infix:<*>', 
        'parenthesis' 
    );
    #print postfix( $match->() );
    #print Dumper $match->();
}

{
    my $match = $grammar->parse( '1infix:<+>1' );
    ok( 
        postfix( $match->() ) eq 
        '1 1 infix:<+>', 
        'operator long name' 
    );
    print postfix( $match->() );
}

__END__
{
    # TODO - emit error message
    my $match = $grammar->parse( '{1 2 3}' );
    ok( 
        postfix( $match->() ) eq 
        '1 2 3', 
        'syntax error' 
    );
    print postfix( $match->() );
    print Dumper $match->();
}

{
    # TODO - remove 'null statements'
    my $match = $grammar->parse( '{1;2;3}' );
    ok( 
        postfix( $match->() ) eq 
        '1 2 3', 
        'stamements inside a block' 
    );
    print postfix( $match->() );
    print Dumper $match->();
}

=for TODO
test lexical add_op:
    $statement->add_op( {
        fixity => 'infix', name => '*', assoc => 'left',
        precedence => 'tighter', other => '+',
    } );
=cut

__END__
