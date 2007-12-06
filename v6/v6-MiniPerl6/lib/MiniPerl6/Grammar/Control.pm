
use v6-alpha;

grammar MiniPerl6::Grammar {


token control {
    | <ctrl_return> { make $$<ctrl_return> }   # return 123;
    | <ctrl_leave>  { make $$<ctrl_leave>  }   # last; break;
    | <if>     { make $$<if>     }   # 1 ?? 2 !! 3
    | <when>   { make $$<when>   }   # when 3 { ... }
    | <for>    { make $$<for>    }   # $x.map(-> $i {...})
    | <while>  { make $$<while>  }   # while ... { ... }
    | <apply>  { make $$<apply>  }   # $obj($arg1, $arg2)
 #  | <call>   { make $$<call>   }   # $obj.method($arg1, $arg2)
};

token if {
    if <.ws>  <exp>  <.opt_ws>
    \{ <.opt_ws> <exp_stmts> <.opt_ws> \} 
    [
        <.opt_ws>
        else <.opt_ws> 
        \{ <.opt_ws> <exp_stmts2> <.opt_ws> \}
        { 
            make ::If( 
                'cond' => $$<exp>, 
                'body' => $$<exp_stmts>, 
                'otherwise' => $$<exp_stmts2>,
            );
        }
    |
        { 
            make ::If( 
                'cond' => $$<exp>, 
                'body' => $$<exp_stmts>, 
                'otherwise' => [ ],
             ) 
        }
    ]
};

token when {
    when <.ws> <exp_seq> <.opt_ws> \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
    { make ::When( 'parameters' => $$<exp_seq>, 'body' => $$<exp_stmts> ) }
};

token for {
    for <.ws> <exp> <.opt_ws> <'->'> <.opt_ws> <var> <.ws> \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
    { make ::For( 'cond' => $$<exp>, 'topic' => $$<var>, 'body' => $$<exp_stmts> ) }
};

token while {
    while <.ws> <exp> <.ws> \{ <.opt_ws> <exp_stmts> <.opt_ws> \}
    { make ::While( 'cond' => $$<exp>, 'body' => $$<exp_stmts> ) }
};

token ctrl_leave {
    leave
    { make ::Leave() }
};

token ctrl_return {
    return <.ws> <exp>
    { make ::Return( 'result' => $$<exp> ) }
    |
    return 
    { make ::Return( 'result' => ::Val::Undef() ) }
};

}
