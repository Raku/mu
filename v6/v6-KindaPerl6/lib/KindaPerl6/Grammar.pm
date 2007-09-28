use v6-alpha;

grammar KindaPerl6::Grammar {

use KindaPerl6::Grammar::Regex;
use KindaPerl6::Grammar::Mapping;
use KindaPerl6::Grammar::Control;
use KindaPerl6::Grammar::Parameters;
use KindaPerl6::Grammar::Term;
use KindaPerl6::Grammar::Statements;
use KindaPerl6::Grammar::Quote;
use KindaPerl6::Grammar::Sub;
use KindaPerl6::Grammar::Token;

my $Class_name;  # for diagnostic messages
sub get_class_name { $Class_name }; 

token ident_digit {
    [ [ <?word> | _ | <?digit> ] <?ident_digit>
    |   <''>
    ]    
};

token ident {
    | [ <!before \d> <?word> | _ ] <?ident_digit>
        [ ':<'   <angle_quoted> '>' | '' ]
    | ¢
};

token full_ident {
    <?ident>
    [   <'::'> <?full_ident>
    |   <''>
    ]    
};

token namespace {
    |   <ident> '::'
        [
        |   <namespace> 
            { return [ $$<ident>, @( $$<namespace> ) ] }
        |   
            { return [ $$<ident> ] }
        ]
    |
        { return [ ] }
};

token to_line_end {
    |  \N <?to_line_end>
    |  <''>
};

token pod_begin {
    |   \n <'=end'> <?to_line_end>
    |   . <?to_line_end> <?pod_begin>
};

token pod_other {
    |   \n <'=cut'> <?to_line_end>
    |   . <?to_line_end> <?pod_other>
};

token ws {
    [
    |    <'#'> <?to_line_end>
    |    \n [
            |  <'=begin'>  <?pod_begin>
            |  <'=kwid'>   <?pod_other>
            |  <'=pod'>    <?pod_other>
            |  <'=for'>    <?pod_other>
            |  <'=head1'>  <?pod_other>
            |  <''>
            ]
    |    \s
    ]
    [ <?ws> | <''> ]
};

token opt_ws  {  <?ws> | <''>  };
token opt_ws2 {  <?ws> | <''>  };
token opt_ws3 {  <?ws> | <''>  };

token parse {
    | <comp_unit>
        [
        |   <parse>
            { return [ $$<comp_unit>, @( $$<parse> ) ] }
        |   { return [ $$<comp_unit> ] }
        ]
    | { return [] }
};

token unit_type {
    <'class'> | <'grammar'> | <'role'> | <'module'>
};

token trait_auxiliary {
    is | does | meta
};

token class_trait {
    <trait_auxiliary> <?ws> <full_ident> 
        { return [ $$<trait_auxiliary>, $$<full_ident> ] }
};

token class_traits {
    | <class_trait>
        [
        |   <?ws> <class_traits>
            { return [ $$<class_trait>, @( $$<class_traits> ) ] }
        |   { return [ $$<class_trait> ] }
        ]
    | { return [] }
};

token comp_unit {
    <?opt_ws> [\; <?opt_ws> | <''> ]
    [ <'use'> <?ws> <'v6-'> <ident> <?opt_ws> \; <?ws>  |  <''> ]
    
    [
    <unit_type> <?opt_ws> <full_ident> <?opt_ws>
    <class_traits> <?opt_ws>
    <'{'>
        { $Class_name := ~$<full_ident> }
        <?opt_ws>
        { 
            COMPILER::add_pad( $Class_name );
        }
        <exp_stmts>
        <?opt_ws>
    <'}'>
    <?opt_ws> [\; <?opt_ws> | <''> ]
    {
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        return ::CompUnit(
            'unit_type'   => $$<unit_type>,
            'name'        => $$<full_ident>,
            'traits'      => $$<class_traits>,
            'attributes'  => { },
            'methods'     => { },
            'body'        => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                body  => $$<exp_stmts>,
            ),
        )
    }
    ] | [
    <?opt_ws>
    {
        $Class_name := 'Main';
        COMPILER::add_pad( $Class_name );
    }
    <exp_stmts2>
    {
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        return ::CompUnit(
            'unit_type'   => 'module',
            'name'        => 'Main',
            'traits'      => [],
            'attributes'  => { },
            'methods'     => { },
            'body'        => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                body  => $$<exp_stmts2>,
            ),
        )
    }
    ]
};

token infix_op {
      <'+'> | <'-'> | <'*'> | <'/'> | eq | ne | <'=='> | <'!='> | <'&&'> | <'||'> | <'~~'> | <'~'> 
    | '<=>'
    | '<=' | '>=' 
    | '<'  | '>' 
    | '&' | '^' | '|'
    | '..'
};

token hyper_op {
    <'>>'> | <''>
};

token prefix_op {
    [ '$' | '@' | '%' | '?' | '!' | '++' | '--' | '+' | '-' | '~' | '|' ] 
    <before '$' | '@' | '%' 
          | '(' | '{' | '[' 
    >
};

token declarator {
     <'my'> | <'state'> | <'has'> | <'our'>
};
token opt_declarator {
    <declarator> <?ws> {return $$<declarator>;} | {return '';}
};

token exp2 { <exp> { return $$<exp> } };



token exp {
    # { say 'exp: going to match <term_meth> at ', $/.to; }
    <term_meth> 
    [
        <?opt_ws>
        <'??'>
        [
          <?opt_ws>  <exp>
          <?opt_ws>  <'!!'>
          <?opt_ws>
          <exp2>
          { 
          
            # XXX TODO - expand macro
            # is &ternary:<?? !!> a macro?
            my $macro_ast := ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'ternary:<?? !!>', namespace => [ ] );
            my $macro := COMPILER::get_var( $macro_ast );
            if defined($macro) {
                # fetch the macro 
                my $sub := ( @COMPILER::PAD[0] ).eval_ast( $macro_ast );
                Main::expand_macro( $sub, $$<term_meth>, $$<exp>, $$<exp2> );
                # say "# ternary macro = ", $sub.perl;
            }
            
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'ternary:<?? !!>', namespace => [ ] ),
                'arguments' => [ $$<term_meth>, $$<exp>, $$<exp2> ],
            ); 
          }
        | { say '*** Syntax error in ternary operation' }
        ]
    |
        <?opt_ws>
        <infix_op>
        <?opt_ws>
        <exp>
          { return ::Apply(
            'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<' ~ $<infix_op> ~ '>', namespace => [ ]  ),
            'arguments' => [ $$<term_meth>, $$<exp> ],
          ) }
    | <?opt_ws> <'::='> <?opt_ws> <exp>
        { 
            my $bind := ::Bind( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>);
            COMPILER::begin_block( $bind );   # ::=   compile-time
            return $bind;                         # :=    run-time
        }
    | <?opt_ws> <':='> <?opt_ws> <exp>
        { return ::Bind( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>) }
    | <?opt_ws> <'='> <?opt_ws> <exp>
        { return ::Assign( 'parameters' => $$<term_meth>, 'arguments' => $$<exp>) }
    |   { return $$<term_meth> }
    ]
};

token opt_ident {  
    | <ident>  { return $$<ident> }
    | <''>     { return 'postcircumfix:<( )>' }
};

token term_meth {
    <full_ident>
    [ \.
        <hyper_op>
        <ident>
            [ \( <?opt_ws> <exp_parameter_list> <?opt_ws> \)
                # { say 'found parameter list: ', $<exp_parameter_list>.perl }
            | \: <?ws> <exp_parameter_list> <?opt_ws>
            |
                {
                    return ::Call(
                        'invocant'  => ::Proto( 'name' => ~$<full_ident> ),
                        'method'    => $$<ident>,
                        'arguments' => undef,
                        'hyper'     => $$<hyper_op>,
                    )
                }
            ]
            {
                return ::Call(
                    'invocant'  => ::Proto( 'name' => ~$<full_ident> ),
                    'method'    => $$<ident>,
                    'arguments' => $$<exp_parameter_list>,
                    'hyper'     => $$<hyper_op>,
                )
            }
    ]
    |
    <term>
    [ \.
        <hyper_op>
        <opt_ident>   # $obj.(42)
            [ \( 
                # { say 'testing exp_parameter_list at ', $/.to }
                <?opt_ws> <exp_parameter_list> <?opt_ws> \)
                # { say 'found parameter list: ', $<exp_parameter_list>.perl }
            | \: <?ws> <exp_parameter_list> <?opt_ws>
            |
                {
                    return ::Call(
                        'invocant'  => $$<term>,
                        'method'    => $$<opt_ident>,
                        'arguments' => undef,
                        'hyper'     => $$<hyper_op>,
                    )
                }
            ]
            {
                return ::Call(
                    'invocant'  => $$<term>,
                    'method'    => $$<opt_ident>,
                    'arguments' => $$<exp_parameter_list>,
                    'hyper'     => $$<hyper_op>,
                )
            }
    | \[ <?opt_ws> <exp> <?opt_ws> \]
         { return ::Index(  'obj' => $$<term>, 'index' => $$<exp> ) }   # $a[exp]
    | \{ <?opt_ws> <exp> <?opt_ws> \}
         { return ::Lookup( 'obj' => $$<term>, 'index' => $$<exp> ) }   # $a{exp}
    | \< <angle_quoted> \>
         { return ::Lookup( 
                'obj' => $$<term>, 
                'index' => ::Val::Buf( 'buf' => ~$<angle_quoted> ),
            ) 
         }   # $a<lit>
    |    { return $$<term> }
    ]
};

token sub_or_method_name {
    <full_ident> [ \. <ident> | <''> ]
};

token opt_type {
    |   [ <'::'> | <''> ]  <full_ident>   { return $$<full_ident> }
    |   <''>                              { return '' }
};

token use_from_perl5 {
    ':from<perl5>' {return 1} | {return 0}
}

#token index { XXX }
#token lookup { XXX }


token sigil { \$ |\% |\@ |\& };

token twigil { [ \. | \! | \^ | \* ] | <''> };

# XXX unused?
# token var_name { <ident> | <'/'> | <digit> };

# used in Term.pm
token undeclared_var {
    <sigil> <twigil> <namespace> <ident>
    {
        # no pre-declaration checks
        return ::Var(
            sigil     => ~$<sigil>,
            twigil    => ~$<twigil>,
            name      => ~$<ident>,
            namespace => $$<namespace>,
        )
    }
};

token var {
    <sigil> '/'
    {
        return 
            ::Var(
                    sigil     => ~$<sigil>,
                    twigil    => '',
                    name      => '/',
                    namespace => [ ],
                )
    }
  |
    <sigil> <twigil> <namespace> <ident>
    {
        # check for pre-declaration
        return COMPILER::get_var(
            ::Var(
                    sigil     => ~$<sigil>,
                    twigil    => ~$<twigil>,
                    name      => ~$<ident>,
                    namespace => $$<namespace>,
                )
        )
    }
};

token val {
    | <val_undef>  { return $$<val_undef> }  # undef
    # | $<exp> := <val_object>   # (not exposed to the outside)
    | <val_int>    { return $$<val_int>   }  # 123
    | <val_bit>    { return $$<val_bit>   }  # True, False
    | <val_num>    { return $$<val_num>   }  # 123.456
    | <val_buf>    { return $$<val_buf>   }  # 'moose'
};

token val_bit {
    | True  { return ::Val::Bit( 'bit' => 1 ) }
    | False { return ::Val::Bit( 'bit' => 0 ) }
};




token val_undef {
    undef <!before \w >
    { return ::Val::Undef( ) }
};

token val_num {  
    XXX { return 'TODO: val_num' } 
};


token digits {  \d  [ <digits> | <''> ]  };


token val_int {
    <digits>
    { return ::Val::Int( 'int' => ~$/ ) }
};


# XXX obsolete?
token exp_seq {
    | <exp>
        # { say 'exp_seq: matched <exp>' }
        [
        |   <?opt_ws> \, <?opt_ws> <exp_seq> 
            <?opt_ws> [ \, <?opt_ws> | <''> ]
            { return [ $$<exp>, @( $$<exp_seq> ) ] }
        |   <?opt_ws> [ \, <?opt_ws> | <''> ]
            { return [ $$<exp> ] }
        ]
    | 
        # { say 'exp_seq: end of match' }
        { return [] }
};


token lit {
    #| <lit_seq>    { return $$<lit_seq>    }  # (a, b, c)
    #| <lit_array>  { return $$<lit_array>  }  # [a, b, c]
    #| <lit_hash>   { return $$<lit_hash>   }  # {a => x, b => y}
    #| <lit_code>   { return $$<lit_code>   }  # sub $x {...}
    | <lit_object> { return $$<lit_object> }  # ::Tree(a => x, b => y);
};

token lit_seq   {  XXX { return 'TODO: lit_seq'    } };
token lit_array {  XXX { return 'TODO: lit_array'  } };
token lit_hash  {  XXX { return 'TODO: lit_hash'   } };
token lit_code  {  XXX { return 'TODO - Lit::Code' } };

token lit_object {
    <'::'>
    <full_ident>
    \( 
    [
        <?opt_ws> <exp_mapping> <?opt_ws> \)
        {
            # say 'Parsing Lit::Object ', $$<full_ident>, ($$<exp_mapping>).perl;
            return ::Lit::Object(
                'class'  => $$<full_ident>,
                'fields' => $$<exp_mapping>
            )
        }
    | { say '*** Syntax Error parsing Constructor ',$$<full_ident>; die() }
    ]
};

#token bind {
#    <exp>  <?opt_ws> <':='> <?opt_ws>  <exp2>
#    {
#        return ::Bind(
#            'parameters' => $$<exp>,
#            'arguments'  => $$<exp2>,
#        )
#    }
#};

token call {
    <exp> \. <ident> \( <?opt_ws> <exp_parameter_list> <?opt_ws> \)
    {
        return ::Call(
            'invocant'  => $$<exp>,
            'method'    => $$<ident>,
            'arguments' => $$<exp_parameter_list>,
        )
    }
};

token apply {
    <namespace> <ident>
    [
        [ \( <?opt_ws> <exp_parameter_list> <?opt_ws> \)
        | <?ws> <exp_parameter_list> <?opt_ws>
        ]
        {
            return ::Apply(
                'code'      => COMPILER::get_var( 
                    ::Var(
                            sigil     => '&',
                            twigil    => '',
                            name      => $$<ident>,
                            namespace => $$<namespace>,
                        ) ),
                'arguments' => $$<exp_parameter_list>,
            )
        }
    |
        {
            return ::Apply(
                'code'      => COMPILER::get_var( 
                    ::Var(
                            sigil     => '&',
                            twigil    => '',
                            name      => $$<ident>,
                            namespace => $$<namespace>,
                        ) ),
                'arguments' => [],
            )
        }
    ]
};

token opt_name {  <ident> | ''  };


token invocant {
    |  <var> \:    { return $$<var> }
    |  { return undef }
};

token capture {
    # TODO - exp_seq / exp_mapping == positional / named 
    # XXX use exp_parameter_list instead
    |  <exp>\:  <?opt_ws> <exp_parameter_list> 
        { return ::Capture( 'invocant' => $$<exp>, 'array' => $$<exp_parameter_list>, 'hash' => [ ] ); }
    |  <exp_mapping> 
        { return ::Capture( 'invocant' => undef, 'array' => [ ], 'hash' => $$<exp_mapping> ); }
        
    # ??? doesn't work here
    #|  <exp_parameter_list>
    #    { return ::Capture( 'invocant' => undef, 'array' => $$<exp_parameter_list>, 'hash' => [ ] ); }
};

token sig {
        <invocant>
        <?opt_ws> 
        # TODO - exp_seq / exp_mapping == positional / named 
        # ??? exp_parameter_list
        <exp_seq> 
        {
            # say ' invocant: ', ($$<invocant>).perl;
            # say ' positional: ', ($$<exp_seq>).perl;
            return ::Sig( 'invocant' => $$<invocant>, 'positional' => $$<exp_seq>, 'named' => { } );
        }
};


token base_class { <full_ident> }

token subset {
    # example:  subset Not_x of Str where { $_ ne 'x' }
    subset  <?ws> 
    <full_ident> <?ws> 
    of      <?ws>
    <base_class> <?ws> 
    where   
    <?opt_ws> \{ <?opt_ws>  
        # { say ' parsing statement list ' }
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in subset \'', get_class_name(), '.', $$<name>, '\' near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        return ::Subset( 
            'name'  => $$<full_ident>, 
            'base_class' => 
                ::Proto( name => $$<base_class> ), 
            'block' => 
                ::Sub( 
                    'name'  => undef, 
                    'block' => ::Lit::Code(
                        pad   => $env,
                        state => { },
                        sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                        body  => $$<exp_stmts>,
                ),
            ),
        );
    }
}


token begin_block {
    BEGIN
    <?opt_ws> \{ <?opt_ws>  

        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in BEGIN near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        my $env := @COMPILER::PAD[0];
        #print "  grammar: dropping pad\n";
        COMPILER::drop_pad();
        #say "BEGIN block";
        #print "  grammar: entering begin block\n";
        return COMPILER::begin_block( 
            # $env, 
            ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token check_block {
    CHECK
    <?opt_ws> \{ <?opt_ws>  
          <exp_stmts> <?opt_ws> 
    [   \}     | { say '*** Syntax Error in CHECK block'; die 'error in Block'; } ]
    { 
        #say "CHECK block";
        return COMPILER::check_block( $$<exp_stmts> );
    }
};

}

=begin

=head1 NAME 

KindaPerl6::Grammar - Grammar for KindaPerl6

=head1 SYNOPSIS

    my $match := $source.parse;
    ($$match).perl;    # generated KindaPerl6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the KindaPerl6 compiler.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2007 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
