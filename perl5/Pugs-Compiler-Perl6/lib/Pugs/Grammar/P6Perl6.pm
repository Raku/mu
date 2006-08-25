# Perl6 implementation of the 'Term' syntax category
# author: Flavio S. Glock - fglock@gmail.com

use v6-alpha;

grammar Pugs::Grammar::Perl6
    does Pugs::Grammar::BaseCategory;

use Pugs::Compiler::Rule;
use Pugs::Runtime::Match;
use Pugs::Grammar::StatementControl;
use Pugs::Grammar::StatementModifier;
use Pugs::Grammar::Expression;
use Pugs::Grammar::Pod;
use Pugs::Grammar::P6Rule; 

token perl6_expression_or_null {
        <Pugs::Grammar::Expression.parse('no_blocks',1)> 
        { return $/{'Pugs::Grammar::Expression.parse'}() }
    |
        { return { null => 1, } }
}

token block {
    \{ <?ws>? <statements> <?ws>? \}
        { 
            #print "matched block\n", Dumper( $/{statements}->data ); 
            return { 
                bare_block => $/{statements}(),
            } 
        }
    |
    \{ <?ws>? \}
        { 
            return { 
                bare_block => { statements => [] } 
            } 
        }
    |
    <'->'>  
        [
            <?ws>? <signature_no_invocant> <?ws>? 
            \{ <?ws>? <statements> <?ws>? \}
            { return { 
                pointy_block => $/{statements}(),
                signature    => $/{signature_no_invocant}(),
            } }
        |
            <?ws>?
            \{ <?ws>? <statements> <?ws>? \}
            { return { 
                pointy_block => $/{statements}(),
                signature    => undef,
            } }
        ]
}

token attribute {
        (<alnum>+) <?ws> ( [<alnum>|_|\\:\\:]+)
        [
            <?ws> <attribute>
            { return [
                    [ { bareword => $/[0]() }, { bareword => $/[1]() } ], 
                    @{$<attribute>()},
            ] }
        |
            { return [[ { bareword => $/[0]() }, { bareword => $/[1]() } ]] }
        ]
    |
        (\\:<alnum>+)
        [
            <?ws>? <attribute>
            { return [
                    [ { bareword => $/[0]() }, { num => 1 } ], 
                    @{$<attribute>()},
            ] }
        |
            { return [[ { bareword => $/[0]() }, { num => 1 } ]] }
        ]
    |
        { return [] }
}

# Str|Num
token signature_term_type {
        <Pugs::Grammar::Term.bare_ident>
        [
            <?ws>? \| <?ws>? <signature_term_type>
            { return {
                op1 => '|',
                exp1 => $/{'Pugs::Grammar::Term.bare_ident'}(),
                exp2 => $<signature_term_type>(),
            } }
        |
            { return $/{'Pugs::Grammar::Term.bare_ident'}() }
        ]
    |
        { return undef }
}

token signature_term_ident {
        # XXX t/subroutines/multidimensional_arglists.t
        \\@ ; <?Pugs::Grammar::Term.ident>
        { return { die => "not implemented" } }
    |
        (  ( <'$'>|<'%'>|<'@'>|<'&'> ) <?Pugs::Grammar::Term.ident> )
        { return $/[0]() }
}

token signature_term {
    <signature_term_type> : <?ws>?
    (<':'>?)
    (<'*'>?)
    <signature_term_ident>
    (<'?'>?)
    (<?ws>? <'='> <Pugs::Grammar::Expression.parse('no_blocks', 1)>)?
    <?ws>? <attribute> 
    { return {
        default    => $/[3] ? $/[3][0]{'Pugs::Grammar::Expression.parse'}() : undef,
        type       => $/{signature_term_type}(),
        name       => $/{signature_term_ident}(),
        attribute  => $/{attribute}(),
        named_only => $/[0](),
        is_slurpy  => $/[1](),
        optional   => $/[2](),
    } }
}

token signature_no_invocant {
        <signature_term>
        [
            <?ws>? <','> <?ws>? <signature_no_invocant>
            { return [
                    $/{signature_term}(), 
                    @{$/{signature_no_invocant}()},
            ] }
        |
            { return [ $/{signature_term}() ] }
        ]
    |
        { return [] }
}

token signature {
        <signature_term> <?ws>? <':'>
        [
            <?ws>? <signature_no_invocant>
            { return [
                    {
                        %{$/{signature_term}()}, 
                        invocant => 1,
                    },
                    @{$/{signature_no_invocant}()},
            ] }
        |
            { return [ 
                    {
                        %{$/{signature_term}()}, 
                        invocant => 1,
                    },
            ] }
        ]
    |
        <signature_no_invocant> 
        { return $/{signature_no_invocant}() }
}

token category_name {
        <Pugs::Grammar::Term.bare_ident> 
        [   <':'> 
            [ 
                <before \{ > 
                <%Pugs::Grammar::Term::hash> 
                { return { 
                    category   => $/{'Pugs::Grammar::Term.bare_ident'}(),
                    name       => {
                        'exp1' => { 
                            'hash' => '%::_V6_GRAMMAR::' . 
                                $/{'Pugs::Grammar::Term.bare_ident'}(), },
                        'exp2' => $/{'Pugs::Grammar::Term::hash'}
                                ()->{bare_block}, 
                        'fixity' => 'postcircumfix',
                        'op1' => { 'op' => '{' },
                        'op2' => { 'op' => '}' },
                    }
                } }
            | 
                #<before \< > 
                <%Pugs::Grammar::Quote::hash>
                { return { 
                    category   => $/{'Pugs::Grammar::Term.bare_ident'}(),
                    name       => {
                        'exp1' => { 
                            'hash' => '%::_V6_GRAMMAR::' . $/{'Pugs::Grammar::Term.bare_ident'}(), },
                        'exp2' => $/{'Pugs::Grammar::Quote::hash'}(), 
                        'fixity' => 'postcircumfix',
                        'op1' => { 'op' => '<' },
                        'op2' => { 'op' => '>' },
                    }
                } }
            ]
        | 
            { return { 
                category   => '',
                name       => $/{'Pugs::Grammar::Term.bare_ident'}(),
            } }
        ]
    |
        { return { 
            category   => '',
            name       => '',
        } }
}

token sub_signature {
        <'('> <?ws>? <signature> <?ws>? <')'>
        { 
            #print "sig ", Dumper( $/{signature}() );
            return $/{signature}() 
        }
    |
        { return [] }
}

token sub_decl {
    [
        ( multi | <null> )           <?ws>?
        ( submethod | method | sub ) <?ws>? 
    |
        ( multi  ) <?ws>?
        ( <null> )
    ]
    <category_name> <?ws>?
    <sub_signature> <?ws>? 
    <attribute>     <?ws>?
    <block>        
    { 
      return { 
        multi      => $/[0](),
        term       => $/[1]() || 'sub',
        category   => $/{category_name}()->{category},
        name       => $/{category_name}()->{name},
        attribute  => $/{attribute}(),
        signature  => $/{sub_signature}(),
        block      => $/{block}(),
    } }
}
    
token rule_decl {
    ( multi | <null> )        <?ws>?
    ( rule  | regex | token ) <?ws>?
    <category_name>  <?ws>?
    <sub_signature>  <?ws>? 
    <attribute>      <?ws>?
    <'{'>            <?ws>?
    [
        <Pugs::Grammar::P6Rule.rule> <?ws>?
        <'}'>
        { return { 
                multi      => $/[0](),
                term       => $/[1](),
                category   => $/{category_name}()->{category},
                name       => $/{category_name}()->{name},
                attribute  => $/{attribute}(),
                signature  => $/{sub_signature}(),
                # pass the match tree to the emitter
                block      => $/{'Pugs::Grammar::P6Rule.rule'}(),
        } }
    |
        # XXX better error messages
        { return { die "invalid rule syntax" } }
    ]
}

# class

token class_decl_name {
    ( class | grammar | module | role | package ) <?ws>? 
    ( <?Pugs::Grammar::Term.cpan_bareword> | <?Pugs::Grammar::Term.bare_ident> | <null> ) 
    { return { 
        statement  => $/[0](),
        name       => $/[1](),
    } }
}

token class_decl {
    <class_decl_name> <?ws>? 
    <attribute>       <?ws>?
    <block>?
    { 
      #print "matched block\n", Dumper( $/{block}[0]->data ); 
      return { 
        term       => $/{class_decl_name}()->{statement},
        name       => $/{class_decl_name}()->{name},
        attribute  => $/{attribute}(),
        block      => defined $/{block}[0]
                      ? $/{block}[0]()
                      : undef ,
    } }
}

# /class

token statement {
    <Pugs::Grammar::StatementControl.parse>
        { 
            return $/->{'Pugs::Grammar::StatementControl.parse'}();
        }
    |
    <Pugs::Grammar::Expression.parse('allow_modifier', 1)> 
        { 
            return $/{'Pugs::Grammar::Expression.parse'}();
        } 
}

token statements {
    [ ; <?ws>? ]*
    [
        <before <'}'> > { $::_V6_SUCCEED = 0 } 
    |
        <statement> 
        <?ws>? [ ; <?ws>? ]*
        [
            <before <'}'> > { $::_V6_SUCCEED = 0 }
        |
            <statements> 
            { return {
                statements => [
                        $/{statement}(),
                        @{ $/{statements}()->{statements} },
                    ]
                }
            }
        |
            { 
            return {
                statements => [ $/{statement}() ],
            } }
        ]
    ]
}

token parse {
    <?ws>? <statements> <?ws>? 
    { return $/{statements}() }
}
