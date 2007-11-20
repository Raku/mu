use v6-alpha;

grammar KindaPerl6::Grammar {

token method_sig {
    |   <.opt_ws> \( <.opt_ws>  <sig>  <.opt_ws>  \)
        { return $$<sig> }
    |   { return ::Sig( 
            'invocant' => ::Var( 
                'sigil'  => '$',
                'twigil' => '',
                'name'   => 'self',
                'namespace' => [ ], 
                ), 
            'positional' => [ ], 
            ) }
};
token sub_sig {
    |   <.opt_ws> \( <.opt_ws>  <sig>  <.opt_ws>  \)
        { return $$<sig> }
    |   { return ::Sig( 
            'invocant' => undef,
            'positional' => [ ], 
            ) }
};

token arrow_sub_sig {
    |   <exp_sig_list>
        { return ::Sig( 
            'invocant' =>   ::Val::Undef(),
            'positional' => $$<exp_sig_list>, 
            ) }
    |   \( <.opt_ws>  <sig>  <.opt_ws>  \)
        { return $$<sig> }
}

token sub {
    sub
    <.ws>  <opt_name>  <.opt_ws> 
    <sub_sig>
    <.opt_ws> \{ 
        <.opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in sub \'', $$<name>, '\': missing closing curly bracket '; die 'error in Block'; } ]
    { 
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<sub_sig>).positional).>>key,
            ]
        );    
        return ::Sub( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token coro {
    coro
    <.ws>  <opt_name>  <.opt_ws> 
    <sub_sig>
    <.opt_ws> \{ 
        <.opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in coro \'', $$<name>, '\': missing closing curly bracket '; die 'error in Block'; } ]
    { 
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<sub_sig>).positional).>>key,
            ]
        );    
        return ::Coro( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token arrow_sub {
    '->'
    <.opt_ws> 
    <arrow_sub_sig>
    <.opt_ws> \{ 
        <.opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in sub: missing closing curly bracket  '; die 'error in Block'; } ]
    { 
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<arrow_sub_sig>).positional).>>key,
            ]
        );    
        return ::Sub( 
            'name'  => undef, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<arrow_sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token bare_block {
    # used by gather { ... }
    # \{ <.opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in Block: missing closing curly bracket  '; die 'error in Block'; } ]
    { 
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                # ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                # @(($$<sub_sig>).positional).>>key,
            ]
        );    
        return ::Lit::Code(
                pad   => $env,
                state => { },
                sig   =>
                    ::Sig( 
                        'invocant' => undef, 
                        'positional' => [ ], 
                    ),
                body  => $$<exp_stmts>,
            );
    }
};

token proto {
    proto <.ws> [ [ method | token | rule | sub ] <.ws> | '' ] 
        <namespace> <ident> <.ws> '{' <.opt_ws> '}'    
        { 
            # proto token x { }
            my $bind := ::Bind(  
                parameters =>         # no pre-declaration checks ???
                        ::Var(
                            sigil     => '&',
                            twigil    => '',
                            name      => ~$<ident>,
                            namespace => $$<namespace>,
                        ), 
                arguments => 
                        ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'new',
                            'invocant'  => ::Proto(
                                name => 'MultiToken',
                            ),
                        ),
            );
            COMPILER::begin_block( $bind );   # ::=   compile-time
            return $bind;                         # :=    run-time
        }  
}

token method {
    method
    <.ws>  <opt_name>  <.opt_ws> 
    <method_sig>
    <.opt_ws> \{ <.opt_ws>  
        # { say ' parsing statement list ' }
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        # { say ' got statement list ', ($$<exp_stmts>).perl } 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<name>, '\' near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        
        my $env   := COMPILER::current_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                ($$<method_sig>).invocant,
                @(($$<method_sig>).positional).>>key,
            ]
        );    
        COMPILER::drop_pad();
        return ::Method( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<method_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};

token multi_method {
    multi <.ws> method 
    
            # multi method { code... }
            # &multi.add_variant( 
            #    method ($a,$b,$c,$d) {
            #        say 'ok 4';
            #    }
            # );

        <.ws>  <namespace> <ident>  <.opt_ws> 
        <method_sig>
        <.opt_ws> \{ <.opt_ws>  
            { 
                COMPILER::add_pad();
            }
            <exp_stmts> 
            <.opt_ws> 
        [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<ident>, '\' near pos=', $/.to; die 'error in Block'; } ]
        {
            # say ' block: ', ($$<exp_stmts>).perl;
            
            my $env   := COMPILER::current_pad();
            KindaPerl6::Grammar::declare_parameters(
                $env,
                [
                    ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                    ($$<method_sig>).invocant,
                    @(($$<method_sig>).positional).>>key,
                ]
            );    
            COMPILER::drop_pad();
            return 
                ::Call(
                    hyper     => '',
                    method   => 'add_variant',
                    invocant => ::Var(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                    ),              
                    arguments => [
                        ::Method( 
                            name  => '',
                            'block' => ::Lit::Code(
                                pad   => $env,
                                state => { },
                                sig   => $$<method_sig>,
                                body  => $$<exp_stmts>,
                            ),
                        ),
                    ],
                );                  
        }  

}

token multi_sub {
    multi <.ws> [ sub <.ws> | '' ]
    
        <namespace> <ident>  <.opt_ws> 
        <sub_sig>
        <.opt_ws> \{ <.opt_ws>  
            { 
                COMPILER::add_pad();
            }
            <exp_stmts> 
            <.opt_ws> 
        [   \}     | { say '*** Syntax Error in sub \'', get_class_name(), ' ', $$<ident>, '\' near pos=', $/.to; die 'error in Block'; } ]
        {
            # say ' block: ', ($$<exp_stmts>).perl;
            
            my $env   := COMPILER::current_pad();
            KindaPerl6::Grammar::declare_parameters(
                $env,
                [
                    ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                    # ($$<sub_sig>).invocant,
                    @(($$<sub_sig>).positional).>>key,
                ]
            );    
            COMPILER::drop_pad();
            return 
                ::Call(
                    hyper     => '',
                    method   => 'add_variant',
                    invocant => ::Var(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                    ),              
                    arguments => [
                        ::Sub( 
                            name  => '',
                            'block' => ::Lit::Code(
                                pad   => $env,
                                state => { },
                                sig   => $$<sub_sig>,
                                body  => $$<exp_stmts>,
                            ),
                        ),
                    ],
                );                  
        }  
}

token token {
    # { say 'parsing Token' }
    token
    <.ws>  <opt_name>  <.opt_ws> \{
        <KindaPerl6::Grammar::Regex.rule>
    \}
    {
        return ::Token(
            name  => ~$<opt_name>,
            regex => $$<KindaPerl6::Grammar::Regex.rule>,
            sym   => undef,
        );
    }
};

token token_sym_ident {

    # TODO - process whitespace in angle_quoted and french_quoted strings

    |  sym \< <angle_quoted>  \>    { return ~$<angle_quoted> }
    |  sym \« <french_quoted> \»    { return ~$<french_quoted> }
    |  <ident>                      { return ~$<ident> }
}

token token_sym {
    # { say 'parsing Token:sym' }
    [ multi <.ws> | '' ]
    token
    <.ws> <namespace> <ident> \: <token_sym_ident> <.opt_ws> \{
        <KindaPerl6::Grammar::Regex.rule>
    \}
    {
            return 
                ::Call(
                    hyper     => '',
                    method   => 'add_token_variant',
                    invocant => ::Var(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                    ),              
                    arguments => [
                        ::Token(
                            name  => undef,   
                            regex => $$<KindaPerl6::Grammar::Regex.rule>,
                            sym   => ~$<token_sym_ident>,
                        ),
                        ::Val::Buf( 'buf' => ~$<token_sym_ident> ),
                    ],
                );                  
    }
};


token macro {
    macro
    <.ws>  <opt_name>  <.opt_ws> 
    <sub_sig>
    <.opt_ws> \{ 
        <.opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in macro \'', $$<name>, '\''; die 'error in Block'; } ]
    { 
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        KindaPerl6::Grammar::declare_parameters(
            $env,
            [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                # ($$<sub_sig>).invocant,
                @(($$<sub_sig>).positional).>>key,
            ]
        );    
        return ::Macro( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $$<exp_stmts>,
            ),
        );
    }
};


};
