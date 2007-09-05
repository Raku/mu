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
    token
    <?ws> <token_p5_modifier> <?ws> \{
        <token_p5_body>
    \}
    {
        return ::Apply(
            code=>::Var(sigil=>'&',twigil=>'',name=>'p5token'),
            arguments=>[::Val::Buf(buf=>$$<token_p5_body>)]
        );
    }
};

}
