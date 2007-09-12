
use v6-alpha;

grammar KindaPerl6::Grammar {

token double_quoted {
    |  \\ .  <double_quoted>
    |  <!before \" | \$ | \@ | \% > . <double_quoted>
    |  <''>    
};

token quoted_exp {
    |  <var>           
        { 
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'prefix:<~>', namespace => [ ] ),
                'arguments' => [ $$<var> ],
            ); 
        }
    |  [ \$ | \@ | \% | '' ] <double_quoted> { return ::Val::Buf( 'buf' => ~$/ ) }
}

token quoted_exp_seq {
    <quoted_exp> 
    [
    |  <before \" >     { return $$<quoted_exp> }
    |  
        <quoted_exp_seq> 
        {        
            return ::Apply(
                'code'      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'infix:<~>', namespace => [ ] ),
                'arguments' => [ $$<quoted_exp>, $$<quoted_exp_seq> ],
            ); 
        }
    ]
}

token single_quoted {
    |  \\ .  <single_quoted>
    |  <!before \' > . <single_quoted>
    |  <''>    
};
token val_buf {
    | \" <quoted_exp_seq> \" { return $$<quoted_exp_seq> }
    | \' <single_quoted>  \' { return ::Val::Buf( 'buf' => ~$<single_quoted> ) }
};

}
