use v6-alpha;

grammar KindaPerl6::Grammar {

token token_p5_modifier {
    ':P5'|':Perl5'
}
token token_p5_body {
    |  \\ .  <token_p5_body>
    |  <!before \} > . <token_p5_body>
    |  <''>    
};
token token_P5 {
    token <?ws> <opt_name> <?opt_ws> <token_p5_modifier> <?opt_ws> \{
        <token_p5_body>
    \}
    {
        my $string := ::Var(
            'sigil'  => '$',
            'twigil' => '',
            'name'   => 'string',
            'namespace' => [ ], 
        );
        my $pos := ::Var(
            'sigil'  => '$',
            'twigil' => '',
            'name'   => 'pos',
            'namespace' => [ ], 
        )
        my $code := [::Apply(
            code=>::Var(sigil=>'&',twigil=>'',name=>'match_p5rx', namespace => [ ] ),
            arguments=>[::Val::Buf(buf=>$$<token_p5_body>),$string,$pos]
        )];
        COMPILER::add_pad();
        my $env  := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        my $sig  := ::Sig( 
            'invocant' => ::Var( 
                'sigil'  => '$',
                'twigil' => '',
                'name'   => 'self',
                'namespace' => [ ], 
            ), 
            'positional' => [$string,$pos], 
            'named' => { }
        );
        KindaPerl6::Grammar::declare_parameters( $env, $code, $sig);    
        return ::Method( 
            'name'  => $$<opt_name>, 
            'block' => ::Lit::Code(
                pad   => $env,
                state => { },
                sig   => $sig,
                body  => $code,
            ),
        );
    }
};

}
