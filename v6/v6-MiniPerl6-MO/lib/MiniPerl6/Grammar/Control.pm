
use v6-alpha;

grammar MiniPerl6::Grammar {


token control {
    | <ctrl_return> { return $$<ctrl_return> }   # return 123;
    | <ctrl_leave>  { return $$<ctrl_leave>  }   # last; break;
    | <if>     { return $$<if>     }   # 1 ?? 2 !! 3
    | <when>   { return $$<when>   }   # when 3 { ... }
    | <for>    { return $$<for>    }   # $x.map(-> $i {...})
    | <while>  { return $$<while>  }   # while ... { ... }
    | <apply>  { return $$<apply>  }   # $obj($arg1, $arg2)
 #  | <call>   { return $$<call>   }   # $obj.method($arg1, $arg2)
};

token if {
    if <?ws>  <exp>  <?opt_ws>
    \{ <?opt_ws> <exp_stmts> <?opt_ws> \} 
    [
        <?opt_ws>
        else <?opt_ws> 
        \{ <?opt_ws> <exp_stmts2> <?opt_ws> \}
        { return ::If( 'cond' => $$<exp>, 'body' => $$<exp_stmts>, 'otherwise' => $$<exp_stmts2> ) }
    |
        { return ::If( 'cond' => $$<exp>, 'body' => $$<exp_stmts>, 'otherwise' => [ ] ) }
    ]
};

token when {
    when <?ws> <exp_seq> <?opt_ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
    { return ::When( 'parameters' => $$<exp_seq>, 'body' => $$<exp_stmts> ) }
};

token for {
    for <?ws> <exp> <?opt_ws> <'->'> <?opt_ws> <var> <?ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
    { return ::For( 'cond' => $$<exp>, 'topic' => $$<var>, 'body' => $$<exp_stmts> ) }
};

token while {
    while <?ws> <exp> <?ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
    { return ::While( 'cond' => $$<exp>, 'body' => $$<exp_stmts> ) }
};

token ctrl_leave {
    leave
    { return ::Leave() }
};

token ctrl_return {
    return <?ws> <exp>
    { return ::Return( 'result' => $$<exp> ) }
    |
    return 
    { return ::Return( 'result' => ::Val::Undef() ) }
};

}
