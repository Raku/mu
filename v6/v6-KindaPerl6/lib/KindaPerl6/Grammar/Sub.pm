use v6-alpha;

grammar KindaPerl6::Grammar {

token method_sig {
    |   <?opt_ws> \( <?opt_ws>  <sig>  <?opt_ws>  \)
        { return $$<sig> }
    |   { return ::Sig( 
            'invocant' => ::Var( 
                'sigil'  => '$',
                'twigil' => '',
                'name'   => 'self' ), 
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

};
