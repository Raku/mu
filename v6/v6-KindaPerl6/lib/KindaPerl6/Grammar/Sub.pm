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

token multi_method {
    multi <?ws> method 
    
            # multi method { code... }
            # (&multi.long_names).push( 
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
                    method   => 'push',
                    invocant => ::Call(
                        hyper     => '',
                        arguments => [ ],
                        method    => 'long_names',
                        invocant  => ::Var(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                        ),      
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
    
            # multi sub { code... }
            # (&multi.long_names).push( 
            #    sub ($a,$b,$c,$d) {
            #        say 'ok 4';
            #    }
            # );

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
                    method   => 'push',
                    invocant => ::Call(
                        hyper     => '',
                        arguments => [ ],
                        method    => 'long_names',
                        invocant  => ::Var(
                            namespace => $$<namespace>,
                            name      => $$<ident>,
                            twigil    => '',
                            sigil     => '&',
                        ),      
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

};
