
use v6-alpha;

grammar MiniPerl6::Grammar {

token key { 
    |  <ident> <before <'=>'> | <.ws> > 
       { make ::Val::Buf( 'buf' => ~$<ident> ) }  # autoquote
    |  <exp>   
       { make $$<exp> } 
};

token pair {
    |   <key> 
        <.opt_ws> <'=>'> <.opt_ws>
        <exp>
        { make [ $$<key>, $$<exp> ] }
    |   \: <sigil> <ident>                  #  :$var
        { 
            make [ 
                ::Val::Buf( 'buf' => ~$<ident> ), 
                ::Var( 'sigil' => ~$$<sigil>, 'twigil' => '', 'name' => $$<ident> ) ] 
        } 
};

token exp_mapping {
    |   <pair> 
        [
        |   <.opt_ws> \, <.opt_ws> <exp_mapping> 
            { make [ $$<pair>, @( $$<exp_mapping> ) ] }
        |   <.opt_ws> [ \, <.opt_ws> | '' ]
            { make [ $$<pair> ] }
        ]
    |
        { make [ ] }
};

}

