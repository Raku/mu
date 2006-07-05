package Pugs::Grammar::Perl6;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Grammar::StatementControl;
use Pugs::Grammar::Expression;
use Pugs::Grammar::Pod;

use Pugs::Compiler::Rule;
use Pugs::Runtime::Match;
use Pugs::Grammar::P6Rule; # our local version of Grammar::Rule.pm

use Data::Dumper;

# TODO - redefine <ws> to test Pod.pm after each \n
# *ws = &Pugs::Grammar::BaseCategory::ws;

sub perl6_expression {
    my $class = shift;
    my $src   = shift;
    my $param = shift;

    #warn "perl6_expression param: ", Dumper @_;

    my ( $ast, $tail ) = Pugs::Grammar::Expression::ast( $src, $param );
    if ( length( $tail ) ) {
        $src = substr( $src, 0, - length( $tail ) );
    }
    return Pugs::Runtime::Match->new( { 
        bool  =>   ( $ast ? 1 : 0 ),
        match =>   $src,
        tail  =>   $tail,
        capture => $ast,
    } )
};

*block = Pugs::Compiler::Regex->compile( q(
    \{ : <?ws>? <statements_or_null> <?ws>? \}
        { return { 
            bare_block => $_[0]{statements_or_null}->(),
        } }
    |
    \-\> : 
        [
            <?ws>? <perl6_expression('no_blocks',0)> <?ws>? 
            \{ <?ws>? <statements_or_null> <?ws>? \}
            { return { 
                pointy_block => $_[0]{statements_or_null}->(),
                signature    => $_[0]{perl6_expression}->(),
            } }
        |
            <?ws>?
            \{ <?ws>? <statements_or_null> <?ws>? \}
            { return { 
                pointy_block => $_[0]{statements_or_null}->(),
                signature    => undef,
            } }
        ]
),
    { grammar => __PACKAGE__ }
)->code;


*if = Pugs::Compiler::Regex->compile( q(
    (if|unless) : <?ws>? 
    $<exp1> := <perl6_expression('no_blocks',0)> <?ws>?
    $<exp2> := <block>        
        [
            <?ws>? else <?ws>? 
            $<exp3> := <block>
                { return { 
                    statement => $_[0][0]->(),
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
                    exp3 => $_[0]{exp3}->(),
                } }
        |
            <?ws>? elsif <?ws>? 
            $<exp3> := <perl6_expression('no_blocks',0)> <?ws>?
            $<exp4> := <block>
            [
                <?ws>? else <?ws>? 
                $<exp5> := <block>
                { return { 
                    statement => $_[0][0]->(),
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
                    exp3 => $_[0]{exp3}->(),
                    exp4 => $_[0]{exp4}->(),
                    exp5 => $_[0]{exp5}->(),
                } }
                
                # TODO: elsif ...
            |
                { return { 
                    statement => $_[0][0]->(),
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
                    exp3 => $_[0]{exp3}->(),
                    exp4 => $_[0]{exp4}->(),
                } }
            ]
        |
            { return { 
                    statement => $_[0][0]->(),
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
            } }
        ]
),
    { grammar => __PACKAGE__ }
)->code;


*for = Pugs::Compiler::Regex->compile( q(
    (for|while) : <?ws>? 
    $<exp1> := <perl6_expression('no_blocks',0)> <?ws>?
    $<exp2> := <block>        
        { return { 
                    statement => $_[0][0]->(),
                    exp1 => $_[0]{exp1}->(),
                    exp2 => $_[0]{exp2}->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*try = Pugs::Compiler::Regex->compile( q(
    (try) : <?ws>? <block>        
        { return { 
                    fixity => 'prefix',
                    op1 => { op => 'try' },
                    exp1 => $_[0]{block}->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*attribute = Pugs::Compiler::Regex->compile( q(
        (<alnum>+) <?ws> 
        (<alnum>+)
        [
            <?ws> <attribute>
            { return [
                    [ { bareword => $_[0][0]->() }, { bareword => $_[0][1]->() } ], 
                    @{$<attribute>->()},
            ] }
        |
            { return [[ { bareword => $_[0][0]->() }, { bareword => $_[0][1]->() } ]] }
        ]
    |
        { return [] }
),
    { grammar => __PACKAGE__ }
)->code;


# Str|Num
*signature_term_type = Pugs::Compiler::Regex->compile( q(
        (<alnum>+)
        [
            <?ws>? \| <?ws>? <signature_term_type>
            { return {
                op1 => '|',
                exp1 => $_[0][0]->(), 
                exp2 => $<signature_term_type>->(),
            } }
        |
            { return $_[0][0]->() }
        ]
    |
        { return undef }
),
    { grammar => __PACKAGE__ }
)->code;


*signature_term_ident = Pugs::Compiler::Regex->compile( q(
        (  ( <'$'>|<'%'>|<'@'>|<'&'> )  <alpha>  [<alnum>|_]* )
            { return $_[0][0]->() }
),
    { grammar => __PACKAGE__ }
)->code;


*signature_term = Pugs::Compiler::Regex->compile( q(
        <signature_term_type> <?ws>?
        (<':'>?)
        (<'*'>?)
        <signature_term_ident>
        (<'?'>?)
        <?ws>? <attribute> 
            { return {
                type       => $_[0]{signature_term_type}->(),
                name       => $_[0]{signature_term_ident}->(),
                attribute  => $_[0]{attribute}->(),
                
                named_only => $_[0][0]->(),
                splat      => $_[0][1]->(),
                optional   => $_[0][2]->(),
            } }
    #|
    #    { return undef }
),
    { grammar => __PACKAGE__ }
)->code;


*signature_no_invocant = Pugs::Compiler::Regex->compile( q(
        <signature_term>
        [
            <?ws>? <','> <?ws>? <signature_no_invocant>
            { return [
                    $_[0]{signature_term}->(), 
                    @{$_[0]{signature_no_invocant}->()},
            ] }
        |
            { return [ $_[0]{signature_term}->() ] }
        ]
    |
        { return [] }
),
    { grammar => __PACKAGE__ }
)->code;


*signature = Pugs::Compiler::Regex->compile( q(
        <signature_term> <?ws>? <':'>
        [
            <?ws>? <signature_no_invocant>
            { return [
                    {
                        %{$_[0]{signature_term}->()}, 
                        invocant => 1,
                    },
                    @{$_[0]{signature_no_invocant}->()},
            ] }
        |
            { return [ 
                    {
                        %{$_[0]{signature_term}->()}, 
                        invocant => 1,
                    },
            ] }
        ]
    |
        <signature_no_invocant> 
            { return $_[0]{signature_no_invocant}->()
            }
),
    { grammar => __PACKAGE__ }
)->code;

*sub_decl_name = Pugs::Compiler::Regex->compile( q(
    ( multi | <''> ) <?ws>?
    ( submethod | method | sub ) <?ws>? 
    ( <?Pugs::Grammar::Term.ident>? ) 
        { return { 
            multi      => $_[0][0]->(),
            statement  => $_[0][1]->(),
            name       => $_[0][2]->(),
        } }
    |
    ( multi ) <?ws>?
    ( <?Pugs::Grammar::Term.ident>? ) 
        { return { 
            multi      => $_[0][0]->(),
            statement  => 'sub',
            name       => $_[0][1]->(),
        } }

),
    { grammar => __PACKAGE__ }
)->code;

*sub_decl = Pugs::Compiler::Regex->compile( q(
    <sub_decl_name> <?ws>? 
    [
        # (sig)
        <'('> : <?ws>? <signature> <?ws>? <')'> <?ws>?
        # attr
        <attribute> <?ws>?
        <block>        
        { return { 
            multi      => $_[0]{sub_decl_name}->()->{multi},
            statement  => $_[0]{sub_decl_name}->()->{statement},
            name       => $_[0]{sub_decl_name}->()->{name},
            
            attribute  => $_[0]{attribute}->(),
            signature  => $_[0]{signature}->(),
            block      => $_[0]{block}->(),
        } }
    |
        # no-(sig)
        # attr
        <attribute> <?ws>?
        <block>        
        { return { 
            multi      => $_[0]{sub_decl_name}->()->{multi},
            statement  => $_[0]{sub_decl_name}->()->{statement},
            name       => $_[0]{sub_decl_name}->()->{name},
            
            attribute  => $_[0]{attribute}->(),
            block      => $_[0]{block}->(),
        } }
    ]
),
    { grammar => __PACKAGE__ }
)->code;


*rule_decl_name = Pugs::Compiler::Regex->compile( q(
    ( rule | regex | token ) <?ws>?
    ( <?Pugs::Grammar::Term.ident>? ) 
        { return { 
            multi      => '',       # ??? 'multi rule' exists?
            statement  => $_[0][0]->(),
            name       => $_[0][1]->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*rule_decl = Pugs::Compiler::Regex->compile( q(
    <rule_decl_name> : <?ws>?   
    # TODO: sig
    # TODO: attr
    <'{'> 
        <?ws>?
        # call PCR parser
        #   XXX - Pugs::Grammar::Rule.rule doesn't work yet
        <Pugs::Grammar::P6Rule.rule>     
        <?ws>?
    <'}'>
    { return { 
            multi      => $_[0]{rule_decl_name}->()->{multi},
            statement  => $_[0]{rule_decl_name}->()->{statement},
            name       => $_[0]{rule_decl_name}->()->{name},
            
            #attribute  => $_[0]{attribute}->(),
            #signature  => $_[0]{signature}->(),
            block      => $_[0]{'Pugs::Grammar::P6Rule.rule'}->(),
    } }
),
    { grammar => __PACKAGE__ }
)->code;


*begin_block = Pugs::Compiler::Regex->compile( q(
    (          
   BEGIN 
 | CHECK 
 | INIT 
 | END
 | FIRST
 | ENTER
 | LEAVE
 | KEEP
 | UNDO
 | NEXT
 | LAST
 | PRE
 | POST
 | CATCH
 | CONTROL
    ) : <?ws>? <block>        
        { return { 
            trait  => $_[0][0]->(),
            %{ $_[0]{block}->() },
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*statement = Pugs::Compiler::Regex->compile( q(
    <begin_block>
        { return $_[0]{begin_block}->();
        }
    |
    <rule_decl>
        { return $_[0]{rule_decl}->();
        }
    |
    <sub_decl>
        { return $_[0]{sub_decl}->();
        }
    |
    <block>
        { return $_[0]{block}->();
        }
    |
    <if>
        { return $_[0]{if}->();
        }
    |
    <for>
        { return $_[0]{for}->();
        }
    |
    <try>   # this actually don't belong here
        { return $_[0]{try}->();
        }
    |
    <perl6_expression> 
        [
            <?ws>? (if|unless|for) <?ws>?
            $<exp1> := <perl6_expression> 
            { return {
                statement => $_[0][0]->(),
                exp2 => $_[0]{perl6_expression}->(),
                exp1 => $_[0]{exp1}->(),
            } } 
        |
            { return $_[0]{perl6_expression}->();
            } 
        ]

),
    { grammar => __PACKAGE__ }
)->code;

*statements = Pugs::Compiler::Regex->compile( q(
    [ ; <?ws>? ]*
    [
        <statement> :
        [
            <?ws>? [ ; <?ws>? ]*
            <statements> 
            { return {
                statements => [
                        $_[0]{statement}->(),
                        @{ $_[0]{statements}->()->{statements} },
                    ]
                }
            }
        |
            { return {
                statements => [ $_[0]{statement}->() ],
            } }
        ]
    |
        { return {
            statements => [],
        } }
    ]
),
    { grammar => __PACKAGE__ }
)->code;

*statements_or_null = Pugs::Compiler::Regex->compile( q(
    <statements> 
        { return $_[0]{statements}->() }
    |
        { return {
            statements => [],
        } }
),
    { grammar => __PACKAGE__ }
)->code;

*parse = Pugs::Compiler::Regex->compile( q(
    <?ws>? 
    <statements_or_null> 
    <?ws>? 
        { return $_[0]{statements_or_null}->() }
),
    { grammar => __PACKAGE__ }
)->code;

1;
__END__
