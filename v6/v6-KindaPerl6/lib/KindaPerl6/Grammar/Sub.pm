use v6-alpha;

grammar KindaPerl6::Grammar {

token method_sig {
    |   <?opt_ws> \( <?opt_ws>  <sig>  <?opt_ws>  \)
        { return $$<sig> }
    |   { return ::Sig( 
            'invocant' => ::Var( 
                'sigil'  => '$',
                'twigil' => '',
                'name'   => 'self',
                'namespace' => [ ], 
                ), 
            'positional' => [ ], 
            'named' => { } ) }
};
token sub_sig {
    |   <?opt_ws> \( <?opt_ws>  <sig>  <?opt_ws>  \)
        { return $$<sig> }
    |   { return ::Sig( 
            'invocant' => undef,
            'positional' => [ ], 
            'named' => { } ) }
};

token arrow_sub_sig {
    |   <undeclared_var>
        { return ::Sig( 
            'invocant' => ::Val::Undef(),
            'positional' => [ $$<undeclared_var> ], 
            'named' => { } ) }
    |   \( <?opt_ws>  <sig>  <?opt_ws>  \)
        { return $$<sig> }
}

token sub {
    sub
    <?ws>  <opt_name>  <?opt_ws> 
    <sub_sig>
    <?opt_ws> \{ 
        <?opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in sub \'', $$<name>, '\''; die 'error in Block'; } ]
    { 
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        my $block := $$<exp_stmts>;
        KindaPerl6::Grammar::declare_parameters(
            $env,
            $block,
            $$<sub_sig>,
        );    
        return ::Sub( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $block,
            ),
        );
    }
};

token coro {
    coro
    <?ws>  <opt_name>  <?opt_ws> 
    <sub_sig>
    <?opt_ws> \{ 
        <?opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in coro \'', $$<name>, '\''; die 'error in Block'; } ]
    { 
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        my $block := $$<exp_stmts>;
        KindaPerl6::Grammar::declare_parameters(
            $env,
            $block,
            $$<sub_sig>,
        );    
        return ::Coro( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $block,
            ),
        );
    }
};

token arrow_sub {
    '->'
    <?opt_ws> 
    <arrow_sub_sig>
    <?opt_ws> \{ 
        <?opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in sub '; die 'error in Block'; } ]
    { 
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        my $block := $$<exp_stmts>;
        KindaPerl6::Grammar::declare_parameters(
            $env,
            $block,
            $$<arrow_sub_sig>,
        );    
        return ::Sub( 
            'name'  => undef, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<arrow_sub_sig>,
                body  => $block,
            ),
        );
    }
};

token proto {
    proto <?ws> [ [ method | token | rule | sub ] <?ws> | '' ] 
        <namespace> <ident> <?ws> '{' <?opt_ws> '}'    
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
                                name => 'Multi',
                            ),
                        ),
            );
            COMPILER::begin_block( $bind );   # ::=   compile-time
            return $bind;                         # :=    run-time
        }  
}

token method {
    method
    <?ws>  <opt_name>  <?opt_ws> 
    <method_sig>
    <?opt_ws> \{ <?opt_ws>  
        # { say ' parsing statement list ' }
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        # { say ' got statement list ', ($$<exp_stmts>).perl } 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<name>, '\' near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        
        my $env   := @COMPILER::PAD[0];
        my $block := $$<exp_stmts>;
        KindaPerl6::Grammar::declare_parameters(
            $env,
            $block,
            $$<method_sig>,
        );    
        COMPILER::drop_pad();
        return ::Method( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<method_sig>,
                body  => $block,
            ),
        );
    }
};

token multi_method {
    multi <?ws> method 
    
            # multi method { code... }
            # &multi.add_variant( 
            #    method ($a,$b,$c,$d) {
            #        say 'ok 4';
            #    }
            # );

        <?ws>  <namespace> <ident>  <?opt_ws> 
        <method_sig>
        <?opt_ws> \{ <?opt_ws>  
            { 
                COMPILER::add_pad();
            }
            <exp_stmts> 
            <?opt_ws> 
        [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<ident>, '\' near pos=', $/.to; die 'error in Block'; } ]
        {
            # say ' block: ', ($$<exp_stmts>).perl;
            
            my $env   := @COMPILER::PAD[0];
            my $block := $$<exp_stmts>;
            KindaPerl6::Grammar::declare_parameters(
                $env,
                $block,
                $$<method_sig>,
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
                                body  => $block,
                            ),
                        ),
                    ],
                );                  
        }  

}

token multi_sub {
    multi <?ws> [ sub <?ws> | '' ]
    
        <namespace> <ident>  <?opt_ws> 
        <sub_sig>
        <?opt_ws> \{ <?opt_ws>  
            { 
                COMPILER::add_pad();
            }
            <exp_stmts> 
            <?opt_ws> 
        [   \}     | { say '*** Syntax Error in sub \'', get_class_name(), ' ', $$<ident>, '\' near pos=', $/.to; die 'error in Block'; } ]
        {
            # say ' block: ', ($$<exp_stmts>).perl;
            
            my $env   := @COMPILER::PAD[0];
            my $block := $$<exp_stmts>;
            KindaPerl6::Grammar::declare_parameters(
                $env,
                $block,
                $$<sub_sig>,
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
                                body  => $block,
                            ),
                        ),
                    ],
                );                  
        }  
}

token token {
    # { say 'parsing Token' }
    token
    <?ws>  <opt_name>  <?opt_ws> \{
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
    [ multi <?ws> | '' ]
    token
    <?ws> <namespace> <ident> \: <token_sym_ident> <?opt_ws> \{
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
    <?ws>  <opt_name>  <?opt_ws> 
    <sub_sig>
    <?opt_ws> \{ 
        <?opt_ws>  
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    [   \}     | { say '*** Syntax Error in macro \'', $$<name>, '\''; die 'error in Block'; } ]
    { 
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        my $block := $$<exp_stmts>;
        KindaPerl6::Grammar::declare_parameters(
            $env,
            $block,
            $$<sub_sig>,
        );    
        return ::Macro( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $$<sub_sig>,
                body  => $block,
            ),
        );
    }
};


};
