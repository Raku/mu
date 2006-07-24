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

*perl6_expression_or_null = Pugs::Compiler::Regex->compile( q(
    <perl6_expression('no_blocks',0)> 
        { return $_[0]{perl6_expression}->() }
    |
        { return {
            null => 1,
        } }
),
    { grammar => __PACKAGE__ }
)->code;

*block = Pugs::Compiler::Regex->compile( q(
    \{ : <?ws>? <statements_or_null> <?ws>? \}
        { return { 
            bare_block => $_[0]{statements_or_null}->(),
        } }
    |
    \-\> : 
        [
            <?ws>? <signature_no_invocant> <?ws>? 
            \{ <?ws>? <statements_or_null> <?ws>? \}
            { return { 
                pointy_block => $_[0]{statements_or_null}->(),
                signature    => $_[0]{signature_no_invocant}->(),
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
    (for|while|until) : <?ws>? 
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

*loop = Pugs::Compiler::Regex->compile( q(
    (loop) : <?ws>?
        [
          <'('> $<exp1> := <perl6_expression_or_null> <?ws>? <';'>
                $<exp2> := <perl6_expression_or_null> <?ws>? <';'>
                $<exp3> := <perl6_expression_or_null> <?ws>? <')'> 
                <?ws>? $<content> := <block>
          { return { statement => $_[0][0]->(),
                     exp1      => $_[0]{exp1}->(),
                     exp2      => $_[0]{exp2}->(),
                     exp3      => $_[0]{exp3}->(),
                     content   => $_[0]{content}->() }
          }
        |
          <block> <?ws>?
          { return { statement => $_[0][0]->(),
                     content   => $_[0]{block}->() }
          }
        |
            # XXX better error messages
            { return { die "invalid loop syntax" } }
       ]
),
    { grammar => __PACKAGE__ }
)->code;

*repeat = Pugs::Compiler::Regex->compile( q(
    (repeat) : <?ws>?
        [
          (while|until) : <?ws>? <perl6_expression('no_blocks',0)>
          <block> <?ws>?
          { return { statement => $_[0][0]->(),
                     which     => $_[0][1]->(),
                     exp2      => $_[0]{perl6_expression}->(),
                     postfix   => 1,
                     content   => $_[0]{block}->() }
          }
        |
          <block> <?ws>?
          (while|until) : <?ws>? <perl6_expression('no_blocks',0)>
          { return { statement => $_[0][0]->(),
                     which     => $_[0][1]->(),
                     exp2      => $_[0]{perl6_expression}->(),
                     postfix   => 1,
                     content   => $_[0]{block}->() }
          }
        |
            # XXX better error messages
            { return { die "invalid repeat syntax" } }
       ]
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
        (<alnum>+) <?ws> ( [<alnum>|_|\\:\\:]+)
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
        (\\:<alnum>+)
        [
            <?ws>? <attribute>
            { return [
                    [ { bareword => $_[0][0]->() }, { num => 1 } ], 
                    @{$<attribute>->()},
            ] }
        |
            { return [[ { bareword => $_[0][0]->() }, { num => 1 } ]] }
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
            # XXX t/subroutines/multidimensional_arglists.t
            \\@ ; <?Pugs::Grammar::Term.ident>
            { return { die => "not implemented" } }
     |
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
        (<?ws>? <'='> <perl6_expression('no_blocks', 0)>)?
        <?ws>? <attribute> 
            { return {
                default    => $_[0][3] ? $_[0][3][0]{perl6_expression}->() : undef,
                type       => $_[0]{signature_term_type}->(),
                name       => $_[0]{signature_term_ident}->(),
                attribute  => $_[0]{attribute}->(),
                
                named_only => $_[0][0]->(),
                is_slurpy  => $_[0][1]->(),
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
    ( my | <''> ) <?ws>?
    ( multi | <''> ) <?ws>?
    ( submethod | method | sub ) <?ws>? 
    ( <?Pugs::Grammar::Term.ident>? ) 
        { return { 
            my         => $_[0][0]->(),
            multi      => $_[0][1]->(),
            statement  => $_[0][2]->(),
            name       => $_[0][3]->(),
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

*sub_signature = Pugs::Compiler::Regex->compile( q(
        # (sig)
        <'('> : <?ws>? <signature> <?ws>? <')'>
        { return $_[0]{signature}->() }
    |
        { return [] }
),
    { grammar => __PACKAGE__ }
)->code;


*sub_decl = Pugs::Compiler::Regex->compile( q(
    <sub_decl_name> <?ws>? 
        # (sig)
        <sub_signature> <?ws>? 
        # attr
        <attribute> <?ws>?
        <block>        
        { return { 
            multi      => $_[0]{sub_decl_name}->()->{multi},
            my         => $_[0]{sub_decl_name}->()->{my},
            statement  => $_[0]{sub_decl_name}->()->{statement},
            name       => $_[0]{sub_decl_name}->()->{name},
            
            attribute  => $_[0]{attribute}->(),
            signature  => $_[0]{sub_signature}->(),
            block      => $_[0]{block}->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*rule_decl_name = Pugs::Compiler::Regex->compile( q(
    ( multi | <''> ) <?ws>?
    ( rule | regex | token ) <?ws>?
    ( <?Pugs::Grammar::Term.ident>? ) 
        { return { 
            multi      => $_[0][0]->(),
            statement  => $_[0][1]->(),
            name       => $_[0][2]->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*rule_decl = Pugs::Compiler::Regex->compile( q(
    <rule_decl_name> : <?ws>?   
        # (sig)
        <sub_signature> <?ws>? 
        # attr
        <attribute> <?ws>?
    <'{'>  
        <?ws>?
    [
        <Pugs::Grammar::P6Rule.rule>     
        <?ws>?
    <'}'>
    { return { 
            multi      => $_[0]{rule_decl_name}->()->{multi},
            statement  => $_[0]{rule_decl_name}->()->{statement},
            name       => $_[0]{rule_decl_name}->()->{name},
            
            attribute  => $_[0]{attribute}->(),
            signature  => $_[0]{sub_signature}->(),

            # pass the match tree to the emitter
            block      => $_[0]{'Pugs::Grammar::P6Rule.rule'}->(),
    } }
    
    |
        # XXX better error messages
        { return { die "invalid rule syntax" } }
    ]
),
    { grammar => __PACKAGE__ }
)->code;


# class


*class_decl_name = Pugs::Compiler::Regex->compile( q(
    ( my | <''> ) <?ws>?
    ( class | grammar | module | role | package ) <?ws>? 
    ( <?Pugs::Grammar::Term.cpan_bareword> |
      <?Pugs::Grammar::Term.bare_ident> |
      <''> ) 
        { return { 
            my         => $_[0][0]->(),
            statement  => $_[0][1]->(),
            name       => $_[0][2]->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;


*class_decl = Pugs::Compiler::Regex->compile( q(
    <class_decl_name> <?ws>? 
        # attr
        <attribute> <?ws>?
        ( <block>? )
        { return { 
            my         => $_[0]{class_decl_name}->()->{my},
            statement  => $_[0]{class_decl_name}->()->{statement},
            name       => $_[0]{class_decl_name}->()->{name},
            
            attribute  => $_[0]{attribute}->(),
            block      => $_[0][0]->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;



# /class


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
    use <?ws> v5 <?ws>?; ((.)*?) ; <?ws>? use <?ws> v6 (.)*? ; 
        { return { 
            perl5source => $_[0][0]->() 
        } }
    |
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
    <if>
        { return $_[0]{if}->();
        }
    |
    <for>
        { return $_[0]{for}->();
        }
    |
    <loop>
        { return $_[0]{loop}->();
        }
    |
    <repeat>
        { return $_[0]{repeat}->();
        }
    |
    <try>   # this actually don't belong here
        { return $_[0]{try}->();
        }
    |
    <perl6_expression> 
        [
            <?ws>? (if|unless|for|while|until) <?ws>?
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
    |
    <block>
        { return $_[0]{block}->();
        }

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
