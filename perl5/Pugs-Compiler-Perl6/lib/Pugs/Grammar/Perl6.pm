package Pugs::Grammar::Perl6;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);
use Pugs::Compiler::Rule;
use Pugs::Runtime::Match;
use Pugs::Grammar::StatementControl;
use Pugs::Grammar::StatementModifier;
use Pugs::Grammar::Expression;
# use Pugs::Grammar::Pod;
use Pugs::Grammar::P6Rule;

use Data::Dumper;

*perl6_expression_or_null = Pugs::Compiler::Token->compile( q(
    <Pugs::Grammar::Expression.parse('no_blocks',1)>
        { return $_[0]{'Pugs::Grammar::Expression.parse'}->() }
    |
        { return { null => 1, } }
),
    { grammar => __PACKAGE__ }
)->code;

*block = Pugs::Compiler::Token->compile( q(
    \{ <?ws>? <statements> <?ws>? \}
        {
            #print "matched block", Dumper( $_[0]{statements}->data );
            return {
                bare_block => $_[0]{statements}->(),
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
    \-\>
        [
            <?ws>? <signature_no_invocant> <?ws>?
            \{ <?ws>? <statements> <?ws>? \}
            { return {
                pointy_block => $_[0]{statements}->(),
                signature    => $_[0]{signature_no_invocant}->(),
            } }
        |
            <?ws>?
            \{ <?ws>? <statements> <?ws>? \}
            { return {
                pointy_block => $_[0]{statements}->(),
                signature    => undef,
            } }
        ]
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
*signature_term_type_2 = Pugs::Compiler::Regex->compile( q(
        <Pugs::Grammar::Term.bare_ident>
        [
            <?ws>? \| <?ws>? <signature_term_type_2>
            { return {
                op1 => '|',
                exp1 => $_[0]{'Pugs::Grammar::Term.bare_ident'}->(),
                exp2 => $<signature_term_type_2>->(),
            } }
        |
            { return $_[0]{'Pugs::Grammar::Term.bare_ident'}->() }
        ]
    |
        { return undef }
),
    { grammar => __PACKAGE__ }
)->code;

*signature_term_type = Pugs::Compiler::Token->compile( q(
        <!before 'sub' >   # TODO
        <signature_term_type_2>
        { return $_[0]{'signature_term_type_2'}->() }
    |
        { return undef }
),
    { grammar => __PACKAGE__ }
)->code;

*signature_term_ident = Pugs::Compiler::Token->compile( q(
            # XXX t/subroutines/multidimensional_arglists.t
            \\@ ; <?Pugs::Grammar::Term.ident>
            { return { die => "not implemented" } }
     |
        (  ( '$' | '%' | '@' | '&' ) <?Pugs::Grammar::Term.ident> )
            { return $_[0][0]->() }
),
    { grammar => __PACKAGE__ }
)->code;

*signature_term = Pugs::Compiler::Token->compile( q(
        <signature_term_type> : <?ws>?
        (':'?)
        ('*'?)
        <signature_term_ident>
        ('?'?)
        <?ws>? <attribute>
        [
        |   <?ws>? '='
            [
            |  <?ws>? '<=>'
            { return {
                default    => { double_quoted => '=' },
                type       => $_[0]{signature_term_type}->(),
                name       => $_[0]{signature_term_ident}->(),
                attribute  => $_[0]{attribute}->(),
                named_only => $_[0][0]->(),
                is_slurpy  => $_[0][1]->(),
                optional   => $_[0][2]->(),
            } }
            |  <Pugs::Grammar::Expression.parse('no_blocks', 1, 'no_comma', 1)>
            { return {
                default    => $_[0]{'Pugs::Grammar::Expression.parse'}->(),
                type       => $_[0]{signature_term_type}->(),
                name       => $_[0]{signature_term_ident}->(),
                attribute  => $_[0]{attribute}->(),
                named_only => $_[0][0]->(),
                is_slurpy  => $_[0][1]->(),
                optional   => $_[0][2]->(),
            } }
            ]
        |   ''
        ]
            { return {
                default    => undef,
                type       => $_[0]{signature_term_type}->(),
                name       => $_[0]{signature_term_ident}->(),
                attribute  => $_[0]{attribute}->(),
                named_only => $_[0][0]->(),
                is_slurpy  => $_[0][1]->(),
                optional   => $_[0][2]->(),
            } }
),
    { grammar => __PACKAGE__ }
)->code;

*signature_no_invocant = Pugs::Compiler::Token->compile( q(
        <signature_term>
        [
            <?ws>? ',' <?ws>? <signature_no_invocant>
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

*signature = Pugs::Compiler::Token->compile( q(
        <signature_term> <?ws>? ':'
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
            { return $_[0]{signature_no_invocant}->() }
),
    { grammar => __PACKAGE__ }
)->code;

*category_name = Pugs::Compiler::Token->compile( q(
        <Pugs::Grammar::Term.bare_ident>
        [   ':'
            [
                <before \{ >
                <%Pugs::Grammar::Term::hash>
                { return {
                    category   => $_[0]{'Pugs::Grammar::Term.bare_ident'}->(),
                    name       => {
                        'exp1' => {
                            'hash' => '%::_V6_GRAMMAR::' .
                                $_[0]{'Pugs::Grammar::Term.bare_ident'}->(), },
                        'exp2' => $_[0]{'Pugs::Grammar::Term::hash'}
                                ->()->{bare_block},
                        'fixity' => 'postcircumfix',
                        'op1' => { 'op' => '{' },
                        'op2' => { 'op' => '}' },
                    }
                } }
            |
                #<before \< >
                <%Pugs::Grammar::Quote::hash>
                { return {
                    category   => $_[0]{'Pugs::Grammar::Term.bare_ident'}->(),
                    name       => {
                        'exp1' => {
                            'hash' => '%::_V6_GRAMMAR::' . $_[0]{'Pugs::Grammar::Term.bare_ident'}->(), },
                        'exp2' => $_[0]{'Pugs::Grammar::Quote::hash'}->(),
                        'fixity' => 'postcircumfix',
                        'op1' => { 'op' => '<' },
                        'op2' => { 'op' => '>' },
                    }
                } }
            ]
        |
            { return {
                category   => '',
                name       => $_[0]{'Pugs::Grammar::Term.bare_ident'}->(),
            } }
        ]
    |
        { return {
            category   => '',
            name       => '',
        } }
),
    { grammar => __PACKAGE__ }
)->code;

*sub_signature = Pugs::Compiler::Token->compile( q(
        '(' <?ws>? <signature> <?ws>? ')'
        {
            #print "sig ", Dumper( $_[0]{signature}->() );
            return $_[0]{signature}->()
        }
    |
        { return [] }
),
    { grammar => __PACKAGE__ }
)->code;

*sub_decl = Pugs::Compiler::Token->compile( q(
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
            multi      => $_[0][0]->(),
            term       => $_[0][1]->() || 'sub',
            category   => $_[0]{category_name}->()->{category},
            name       => $_[0]{category_name}->()->{name},
            attribute  => $_[0]{attribute}->(),
            signature  => $_[0]{sub_signature}->(),
            block      => $_[0]{block}->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;

*rule_decl = Pugs::Compiler::Token->compile( q(
    ( multi | <null> )        <?ws>?
    ( rule  | regex | token ) <?ws>?
    <category_name>  <?ws>?
    <sub_signature>  <?ws>?
    <attribute>      <?ws>?
    '{'            <?ws>?
    [
        <Pugs::Grammar::P6Rule.rule> <?ws>?
        '}'
        { return {
                multi      => $_[0][0]->(),
                term       => $_[0][1]->(),
                category   => $_[0]{category_name}->()->{category},
                name       => $_[0]{category_name}->()->{name},
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

*proto_rule_decl = Pugs::Compiler::Token->compile( q(
    proto <?ws>
    ( multi | <null> )        <?ws>?
    ( rule  | regex | token ) <?ws>?
    <category_name>  <?ws>?
    <sub_signature>  <?ws>?
    <attribute>      <?ws>?
    '{'    <?ws>?  [ '...' <?ws>? | '' ]  '}'
    # TODO
        { return { int => 0 ,} }
),
    { grammar => __PACKAGE__ }
)->code;

# class

*class_decl_name = Pugs::Compiler::Token->compile( q(
    ( class | grammar | module | role | package ) <?ws>?
    ( <?Pugs::Grammar::Term.cpan_bareword> | <?Pugs::Grammar::Term.bare_ident> | <null> )
        { return {
            statement  => $_[0][0]->(),
            name       => $_[0][1]->(),
        } }
),
    { grammar => __PACKAGE__ }
)->code;

*class_decl = Pugs::Compiler::Token->compile( q(
    <class_decl_name> <?ws>?
    <attribute>       <?ws>?
    <block>?
        {
          #print "matched block\n", Dumper( $_[0]{block}[0]->data );
          return {
            term       => $_[0]{class_decl_name}->()->{statement},
            name       => $_[0]{class_decl_name}->()->{name},
            attribute  => $_[0]{attribute}->(),
            block      => defined $_[0]{block}[0]
                          ? $_[0]{block}[0]->()
                          : undef ,
        } }
),
    { grammar => __PACKAGE__ }
)->code;

# /class

*statement = Pugs::Compiler::Token->compile( q(
    <Pugs::Grammar::StatementControl.parse>
        {
            return $/->{'Pugs::Grammar::StatementControl.parse'}->();
        }
    |
    <Pugs::Grammar::Expression.parse('allow_modifier', 1)>
        {
            return $_[0]{'Pugs::Grammar::Expression.parse'}->();
        }
),
    { grammar => __PACKAGE__ }
)->code;

*statements = Pugs::Compiler::Token->compile( q(
    [ ; <?ws>? ]*
    [
        <!before '}' >
        <statement>
        #{ print "02 end statement", Dumper( $_[0]{statement}->() ) }
        <?ws>? [ ; <?ws>? ]*
        [
            <!before '}' >
            #{ print "04 another statement\n"; }
            <statements>
            {
                #{ print "06 return statements\n"; }
                return {
                    statements => [
                        $_[0]{statement}->(),
                        @{ $_[0]{statements}->()->{statements} },
                    ]
                }
            }
        |
            {
                #{ print "08 return statements\n"; }
                return {
                    statements => [ $_[0]{statement}->() ],
            } }
        ]
    |
            {
                #{ print "10 return statements\n"; }
                return {
                    statements => [],
            } }
    ]
),
    { grammar => __PACKAGE__ }
)->code;

*parse = Pugs::Compiler::Token->compile( q(
    <?ws>? <statements> <?ws>?
        { return $_[0]{statements}->() }
),
    { grammar => __PACKAGE__ }
)->code;

1;
__END__
