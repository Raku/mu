
use v6-alpha;

grammar KindaPerl6::Grammar {

token double_quoted {
    |  \\ .  <double_quoted>
    |  <!before \" > . <double_quoted>
    |  <''>    
};

token single_quoted {
    |  \\ .  <single_quoted>
    |  <!before \' > . <single_quoted>
    |  <''>    
};
token val_buf {
    | \" <double_quoted>  \" { return ::Val::Buf( 'buf' => ~$<double_quoted> ) }
    | \' <single_quoted>  \' { return ::Val::Buf( 'buf' => ~$<single_quoted> ) }
};

}
