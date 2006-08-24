# Perl6 implementation of the 'Term' syntax category
# author: Flavio S. Glock - fglock@gmail.com

use v6-alpha;

grammar Pugs::Grammar::Term
    does Pugs::Grammar::BaseCategory;

token ident {
        \!      # $!
    |   \??     # $?CALLER
        \*?     # $*x
        # \.?     # $.x  - XXX causes problems with 1..5 for some reason
        \:?     # $:x
        [
            [ <'::'> | <null> ]
            [ _ | <?alpha> ]
            [ _ | <?alnum> ]*
        ]+
    |   <before \< | \[ | \{ >   # $<thing> == $/<thing>; $[thing] = $/[thing]
    |   \/      # $/
}

token bare_ident {
    [
        [ <'::'> | <null> ]
        [ _ | <?alpha> ]
        [ _ | <?alnum> ]*
    ]+
}

token parenthesis {
        <?ws>? <Pugs::Grammar::Expression.parse('allow_semicolon', 1)> <?ws>? 
        <')'>
        { return {
            op1 => { op => "(" },
            op2 => { op => ")" },
            fixity => "circumfix",
            exp1 => $/{'Pugs::Grammar::Expression.parse'}() 
        } }
    |
        <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? 
        <')'>
        { return {
            op1 => { op => "(" },
            op2 => { op => ")" },
            fixity => "circumfix",
            exp1 => $/{'Pugs::Grammar::Perl6.block'}() 
        } }
    |
        <?ws>? 
        <')'>
        { return {
            op1 => { op => "(" },
            op2 => { op => ")" },
            fixity => "circumfix",
        } }
}

token brackets {
        <Pugs::Grammar::Infix.parse> 
        <']'>
        { return {
            op => $/{'Pugs::Grammar::Infix.parse'}(),
            reduce => 1, 
        } }
    |
        <?ws>? <Pugs::Grammar::Expression.parse> <?ws>? 
        <']'>
        { return {
            op1 => { op => "[" },
            op2 => { op => "]" },
            fixity => "circumfix",
            exp1 => $/{'Pugs::Grammar::Expression.parse'}() 
        } }
    |
        <?ws>? <Pugs::Grammar::Perl6.block> <?ws>? 
        <']'>
        { return {
            op1 => { op => "[" },
            op2 => { op => "]" },
            fixity => "circumfix",
            exp1 => $/{'Pugs::Grammar::Perl6.block'}() 
        } }
    |
        <?ws>? 
        <']'>
        { return {
            op1 => { op => "[" },
            op2 => { op => "]" },
            fixity => "circumfix",
        } }
}

token cpan_bareword {
    ( <ident> [ - <ident> ]+ )
}

regex perl5source {
    (.*?) [ ; | <?ws> ] use <?ws> v6 (.)*? ; 
        { return { 
            perl5source => $/[0]() 
        } }
}

token term:<$> {
    [ <?Pugs::Grammar::Term.ident>
      { return { scalar => '$' ~ $/() ,} }
    | (\d+)
      { return { scalar => '$' ~ $/() ,} }
    ] }
token term:<$.> {
    <?Pugs::Grammar::Term.ident>
    { return { scalar => '$.' ~ $/() ,} } }
token term:<$/> {
    { return { scalar => '$/' ,} } }
token term:<$()> {
    { return { 
        'op1' => 'call',
        'sub' => {
            'scalar' => '$/',
        }
    } } }
token term:{'$<'} {
    ( <?Pugs::Grammar::Term.ident> ) \>
    { return { scalar => { match_variable => $/[0]() ,} } } }
token term:<@> {
        # XXX t/subroutines/multidimensional_arglists.t
        \; <?Pugs::Grammar::Term.ident>
        { return { die => "not implemented" } }
    |
        <?Pugs::Grammar::Term.ident>
        { return { array => "\@" ~ $/() ,} } }
token term:<%> {
    <?Pugs::Grammar::Term.ident>
    { return { hash  => "\%" ~ $/() ,} } }
token term:<%(> {
    <Pugs::Grammar::Term.parenthesis>
    { return {
        'exp1' => $/{'Pugs::Grammar::Term.parenthesis'}(),
        'fixity' => 'prefix',
        'op1' => { 'op' => 'hash', }
    } } }
token term:<&> {
    <?Pugs::Grammar::Term.ident>
    { return { code  => "\&" ~ $/() ,} } }
token term:<(> {
    <Pugs::Grammar::Term.parenthesis>
    { return $/{'Pugs::Grammar::Term.parenthesis'}() } }
token term:<[> {
    <Pugs::Grammar::Term.brackets>
    { return $/{'Pugs::Grammar::Term.brackets'}() } }
token term:<{> {
        <?ws>? <'}'>
        { return { 
            bare_block => { statements => [] },
        } }
    |
        <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? <'}'>
        { return { 
            bare_block => $/{'Pugs::Grammar::Perl6.statements'}(),
        } } }
token term:{'->'} { 
    [
        <?ws>? <Pugs::Grammar::Perl6.signature_no_invocant> <?ws>? 
        \{ <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? \}
        { return { 
            pointy_block => $/{'Pugs::Grammar::Perl6.statements'}(),
            signature    => $/{'Pugs::Grammar::Perl6.signature_no_invocant'}(),
        } }
    |
        <?ws>?
        \{ <?ws>? <Pugs::Grammar::Perl6.statements> <?ws>? \}
        { return { 
            pointy_block => $/{'Pugs::Grammar::Perl6.statements'}(),
            signature    => undef,
        } }
    ] }
token term:<.> {
    # .method op
    <?Pugs::Grammar::Term.ident>
    { return { dot_bareword  => $/() ,} } }
token term:<...> {
    { return { term => "yada" } } }
token term:<self> {
    { return { term => "self" } } }
token term:<undef> {
    { return { term => "undef" } } }
token term:<my> {
    <?ws> <Pugs::Grammar::Term.parse>
    <?ws>? <Pugs::Grammar::Perl6.attribute>
    { return { 
        exp1 => $/{'Pugs::Grammar::Term.parse'}(),
        attribute  => $/{'Pugs::Grammar::Perl6.attribute'}(),
        variable_declarator => "my",
    } } }
token term:<our> {
    <?ws> <Pugs::Grammar::Term.parse>
    <?ws>? <Pugs::Grammar::Perl6.attribute>
    { return { 
        exp1 => $/{'Pugs::Grammar::Term.parse'}(),
        attribute  => $/{'Pugs::Grammar::Perl6.attribute'}(),
        variable_declarator => "our",
    } } }
token term:<has> {
    <?ws> <Pugs::Grammar::Term.parse>
    <?ws>? <Pugs::Grammar::Perl6.attribute>
    { return { 
        exp1 => $/{'Pugs::Grammar::Term.parse'}(),
        attribute  => $/{'Pugs::Grammar::Perl6.attribute'}(),
        variable_declarator => "has",
    } } }
token term:<state> {
    <?ws> <Pugs::Grammar::Term.parse>
    <?ws>? <Pugs::Grammar::Perl6.attribute>
    { return { 
        exp1 => $/{'Pugs::Grammar::Term.parse'}(),
        attribute  => $/{'Pugs::Grammar::Perl6.attribute'}(),
        variable_declarator => "state",
    } } }
token term:<constant> {
    <?ws> <Pugs::Grammar::Term.parse>
    <?ws>? <Pugs::Grammar::Perl6.attribute>
    { return { 
        exp1 => $/{'Pugs::Grammar::Term.parse'}(),
        attribute  => $/{'Pugs::Grammar::Perl6.attribute'}(),
        variable_declarator => "constant",
    } } }
token term:<s> {
    <Pugs::Grammar::Term.substitution>
    { return { 
        substitution => $/{'Pugs::Grammar::Term.substitution'}(),
    } } }
token term:<rx> {
    <Pugs::Grammar::Term.rx>
    { return { 
        rx => $/{'Pugs::Grammar::Term.rx'}(),
    } } }
token term:<m> {
    <Pugs::Grammar::Term.rx>
    { return { 
        rx => $/{'Pugs::Grammar::Term.rx'}(),
    } } }
token term:</> {
    <Pugs::Grammar::Term.rx_body('open','/')>
    { return { 
        rx => $/{'Pugs::Grammar::Term.rx_body'}(),
    } } }
token term:<perl5:> {
    ### perl5:Test::More
    <Pugs::Grammar::Term.bare_ident> 
    { return { 
        bareword => $/{'Pugs::Grammar::Term.bare_ident'}(),
        lang => 'perl5',
    } } }
token term:<use> {
        # "use v5"
        <?ws> v5 <?ws>?; <perl5source> 
        { return $/{perl5source}() }
    |
        # default
        { return { bareword => 'use' } } }
token term:<do> { 
    # { print "statement do \n"; }
    <?ws> 
    $<exp1> := <Pugs::Grammar::Perl6.statement>        
    { return { 
        statement => 'do',
        exp1 => $/{exp1}(),
    } } }
token term:<:> { 
    ### pair - long:<name> 
        # :foo<bar>
        ([_|\w]+) \< <Pugs::Grammar::Quote.angle_quoted>
        { return {
            pair => { 
                key   => { single_quoted => $/[0]() }, 
                value => { single_quoted => $/{'Pugs::Grammar::Quote.angle_quoted'}() }, 
        } } }
    |
        # :foo(exp)
        ([_|\w]+) \(  
            <?ws>? <Pugs::Grammar::Expression.parse> <?ws>? 
        \)
        { return {
            pair => { 
                key   => { single_quoted => $/[0]() }, 
                value => $/{'Pugs::Grammar::Expression.parse'}(), 
        } } }
    |
        # :$foo 
        \$ ((_|\w)+)
        { return {
            pair => { 
                key   => { single_quoted => $/[0]() }, 
                value => { scalar  => '$' ~ $/[0]() }, 
        } } }
    |
        # :foo 
        ((_|\w)+)
        { return {
            pair => { 
                key   => { single_quoted => $/[0]() }, 
                value => { num => 1 }, 
        } } }
    |
        # :!foo 
        <'!'> ((_|\w)+)
        { return {
            pair => { 
                key   => { single_quoted => $/[0]() }, 
                value => { num => 0 }, 
        } } }
    }
token term:{''} { 
        ### num/int
        \d+ 
        [
            \.\d+
            [ <[Ee]> <[+-]>? \d+ ]?
            { return { num => $() ,} } 
        |
            <[Ee]> <[+-]>? \d+ 
            { return { num => $() ,} } 
        |
            { return { int => $() ,} } 
        ]
    |
        <Pugs::Grammar::Perl6.sub_decl>
            { return $/{'Pugs::Grammar::Perl6.sub_decl'}(); }
    |
        <Pugs::Grammar::Perl6.class_decl>
            { return $/{'Pugs::Grammar::Perl6.class_decl'}(); }
    |
        ### Test-0.0.6
        <Pugs::Grammar::Term.cpan_bareword> 
        { return { cpan_bareword => $/{'Pugs::Grammar::Term.cpan_bareword'}() } }
    |
        ### Test::More
        <Pugs::Grammar::Term.bare_ident> 
        { return { bareword => $/{'Pugs::Grammar::Term.bare_ident'}() } }
    }
token term:<BEGIN> {
    <?ws>? <Pugs::Grammar::Perl6.block>        
        { return { 
            trait  => 'BEGIN',
            %( $/{'Pugs::Grammar::Perl6.block'}() ),
        } }
}
token term:<CHECK> {
    <?ws>? <Pugs::Grammar::Perl6.block>        
        { return { 
            trait  => 'CHECK',
            %( $/{'Pugs::Grammar::Perl6.block'}() ),
        } }
}
token term:<INIT> {
    <?ws>? <Pugs::Grammar::Perl6.block>        
        { return { 
            trait  => 'INIT',
            %( $/{'Pugs::Grammar::Perl6.block'}() ),
        } }
}

token term:<START> { 
    <?ws>? <Pugs::Grammar::Perl6.block>        
        { return { 
            trait  => 'START',
            %( $/{'Pugs::Grammar::Perl6.block'}() ),
        } }
}
token term:<FIRST> {
    <?ws>? <Pugs::Grammar::Perl6.block>        
        { return {
            trait  => 'FIRST',
            %( $/{'Pugs::Grammar::Perl6.block'}() ),
        } }
}
token term:<ENTER> {
    <?ws>? <Pugs::Grammar::Perl6.block>        
        { return { 
            trait  => 'ENTER',
            %( $/{'Pugs::Grammar::Perl6.block'}() ),
        } }
}

token parse {
    <%::_V6_GRAMMAR::term>
    { 
        #print "BaseCategory matched hash ", Dumper( $_[0]->data );
        return $/{'::_V6_GRAMMAR::term'}();
    }
}
