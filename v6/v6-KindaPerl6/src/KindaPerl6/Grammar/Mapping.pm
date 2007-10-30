
use v6-alpha;

grammar KindaPerl6::Grammar {

token exp2 {  <exp> { return $$<exp> }  };

token pair {
    |   <ident>                             #  key => value
        <.opt_ws> '=>' <.opt_ws>
        <exp>
        { return [ ::Val::Buf( 'buf' => ~$<ident> ), $$<exp> ] }
    |   <exp2>                              #  key => value
        <.opt_ws> '=>' <.opt_ws>
        <exp>
        { return [ $$<exp2>, $$<exp> ] }
    |   \: <ident> \< <angle_quoted> \>     #  :key<value>
        { 
            return [ 
                ::Val::Buf( 'buf' => ~$<ident> ), 
                ::Val::Buf( 'buf' => ~$<angle_quoted> ) ] 
        } 
    |   \: <ident> \( <.opt_ws> <exp> <.opt_ws> \)   #  :key(value)
        { 
            return [ 
                ::Val::Buf( 'buf' => ~$<ident> ), 
                $$<exp> ] 
        } 
    |   \: <ident>                          #  :key
        { 
            return [ 
                ::Val::Buf( 'buf' => ~$<ident> ), 
                ::Val::Bit( 'bit' => 1 ) ] 
        } 
    |   \: <sigil> <ident>                  #  :$var
        { 
            return [ 
                ::Val::Buf( 'buf' => ~$<ident> ), 
                ::Var( 'sigil' => ~$$<sigil>, 'twigil' => '', 'name' => $$<ident>, namespace => [ ] ) ] 
        } 
};

token exp_mapping {
    |   <pair> 
        [
        |   <.opt_ws> \, <.opt_ws> <exp_mapping> 
            { return [ $$<pair>, @( $$<exp_mapping> ) ] }
        |   <.opt_ws> [ \, <.opt_ws> | '' ]
            { return [ $$<pair> ] }
        ]
    |
        { return [ ] }
};

}

