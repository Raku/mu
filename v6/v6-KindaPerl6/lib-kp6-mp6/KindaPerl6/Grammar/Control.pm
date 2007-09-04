
use v6-alpha;

grammar KindaPerl6::Grammar {


token control {
    | <ctrl_return> { return $$<ctrl_return> }   # return 123;
    | <ctrl_leave>  { return $$<ctrl_leave>  }   # last; break;
    | <if>     { return $$<if>     }   # 1 ?? 2 !! 3
    | <unless> { return $$<unless> }   # !1 ?? 2 !! 3
    | <when>   { return $$<when>   }   # when 3 { ... }
    | <for>    { return $$<for>    }   # $x.map(-> $i {...})
    | <while>  { return $$<while>  }   # while ... { ... }
    | <apply>  { return $$<apply>  }   # $obj($arg1, $arg2)
 #  | <call>   { return $$<call>   }   # $obj.method($arg1, $arg2)
};

token block1 {
    \{  <?opt_ws> 
        { 
            COMPILER::add_pad();
        }
        <exp_stmts> 
        <?opt_ws> 
    \} 
        {
            my $env := @COMPILER::PAD[0];
            COMPILER::drop_pad();
            return ::Lit::Code(
                    pad   => $env,
                    state => { },
                    sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                    body  => $$<exp_stmts>,
                );
        }
};

token block2 {
    <block1>  
    { return $$<block1> }
};

token if {
    if <?ws>  <exp>  <?opt_ws>
    <block1>
    [
        <?opt_ws>
        else <?opt_ws> 
        <block2>
        { 
            return ::If( 
                'cond'      => $$<exp>, 
                'body'      => $$<block1>, 
                'otherwise' => $$<block2>,
            );
        }
    |
        { 
            return ::If( 
                'cond' => $$<exp>, 
                'body' => $$<block1>, 
                'otherwise' => undef,
             ) 
        }
    ]
};

token unless {
    unless <?ws>  <exp>  <?opt_ws>
    <block1>
    [
        <?opt_ws>
        else <?opt_ws> 
        <block2>
        { 
            return ::If( 
                'cond'      => $$<exp>, 
                'body'      => $$<block2>,
                'otherwise' => $$<block1>,
            );
        }
    |
        { 
            return ::If(
                'cond' => $$<exp>,
                'body' => undef,
                'otherwise' => $$<block1>,
             )
        }
    ]
};

token when {
    when <?ws> <exp_seq> <?opt_ws> <block1>
    { 
        return ::When( 
            'parameters' => $$<exp_seq>, 
            'body'       => $$<block1>,
            ) }
};

token for {
    for <?ws> <exp> <?opt_ws> <'->'> <?opt_ws> <var> 
        { 
            COMPILER::add_pad();
            my $env := @COMPILER::PAD[0];
            push @($env.lexicals),::Decl(type=>'',decl=>'my',var=>$$<var>);
        }
    
    <?ws> <block1>

    { 
            my $env := @COMPILER::PAD[0];
            COMPILER::drop_pad();
            my $block := $$<block1>;
            return ::Lit::Code(
                    pad   => $env,
                    state => { },
                    sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                    body  => [
                        ::For( 
                            'cond'  => $$<exp>, 
                            'topic' => $$<var>, 
                            'body'  => $block,
                            ),                    
                    ],
                );
    }
};

token while {
    while <?ws> <exp> <?ws> <block1>
    { 
        return ::While( 
            'cond' => $$<exp>, 
            'body' => $$<block1>,
            ) }
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
