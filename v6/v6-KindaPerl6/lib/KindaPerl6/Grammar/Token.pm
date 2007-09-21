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
        return ::Token(
             'name' => $$<opt_name>,
             'regex' => ::P5Token(regex => $$<token_p5_body>),
             'sym' => undef
        );
    }
};

}
