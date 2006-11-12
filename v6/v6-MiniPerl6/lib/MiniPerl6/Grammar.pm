use v6-alpha;

grammar MiniPerl6::Grammar {

use MiniPerl6::Grammar::Regex;

# XXX - move to v6.pm emitter
sub array($data)    { use v5; @$data; use v6; };

my $Class_name;  # for diagnostic messages

token full_ident {
    <ident>
    [   <'::'> <full_ident>
    |   <''>
    ]    
};

token to_line_end {
    |  \N <?to_line_end>
    |  <''>
};

token pod_begin {
    |   \n =end <?to_line_end>
    |   . <?to_line_end> <?pod_begin>
};

token pod_other {
    |   \n =cut <?to_line_end>
    |   . <?to_line_end> <?pod_other>
};

token ws {
    [
    |    \# <?to_line_end>
    |    \n = [
            |  begin  <?pod_begin>
            |  kwid   <?pod_other>
            |  pod    <?pod_other>
            |  for    <?pod_other>
            |  head1  <?pod_other>
            ]
    |    \n
    |    \s
    ]
    [ <?ws> | <''> ]
};

token opt_ws {  <?ws> | <''>  };

token parse {
    | <comp_unit>
        [
        |   <?opt_ws> [\; <?opt_ws> | <''> ]  <parse>
            { return [ $$<comp_unit>, array( $$<parse> ) ] }
        |   <?opt_ws> [\; <?opt_ws> | <''> ]
            { return [ $$<comp_unit> ] }
        ]
    | { return [] }
};

token comp_unit {
    <?opt_ws>
    [ use <?ws> v6- <ident> <?opt_ws> \; <?ws>  |  <''> ]
    
    [ class | grammar ]  <?opt_ws> <full_ident> <?opt_ws> \{
        { $Class_name := ~$<full_ident> }
        <?opt_ws>
        <exp_stmts>
        <?opt_ws>
    \}
    <?opt_ws>
    {
        return ::CompUnit(
            'name'        => $$<full_ident>,
            'attributes'  => { },
            'methods'     => { },
            'body'        => $$<exp_stmts>,
        )
    }
};

token exp {
    # { say "exp: going to match <term_meth> at ", $/.to; }
    <term_meth> 
    [
        <?opt_ws>
        $<op> := [ <'??'> ]
        [
          <?opt_ws>  <exp>
          <?opt_ws>  <'!!'>
          <?opt_ws>
          $<exp_3> := <exp>
          { return ::Apply(
            code      => 'ternary:<?? ::>',
            arguments => [ $$<term_meth>, $$<exp>, $$<exp_3> ],
          ) }
        | { say "*** Syntax error in ternary operation" }
        ]
    |
        <?opt_ws>
        $<op> := [ \+ | \- | \* |/ | eq | ne | == | != | \&\& | \|\| | ~~ | ~ ]
        <?opt_ws>
        <exp>
          { return ::Apply(
            code      => 'infix:<' ~ $$<op> ~ '>',
            arguments => [ $$<term_meth>, $$<exp> ],
          ) }
    | <?opt_ws> <':='> <?opt_ws> <exp>
        { return ::Bind(parameters => $$<term_meth>, arguments => $$<exp>) }
    |   { return $$<term_meth> }
    ]
}

token term_meth {
    <full_ident>
    [ \.
        $<hyper> := [ <'>>'> | <''> ]
        <ident>
            [ \( <?opt_ws> <exp_seq> <?opt_ws> \)
                # { say "found parameter list: ", $<exp_seq>.perl }
            | \: <?ws> <exp_seq> <?opt_ws>
            |
                {
                    return ::Call(
                        invocant  => ::Proto( name => ~$<full_ident> ),
                        method    => $$<ident>,
                        arguments => undef,
                        hyper     => $$<hyper>,
                    )
                }
            ]
            {
                return ::Call(
                    invocant  => ::Proto( name => ~$<full_ident> ),
                    method    => $$<ident>,
                    arguments => $$<exp_seq>,
                    hyper     => $$<hyper>,
                )
            }
    ]
    |
    <term>
    [ \.
        $<hyper> := [ <'>>'> | <''> ]
        <ident>
            [ \( 
                # { say "testing exp_seq at ", $/.to }
                <?opt_ws> <exp_seq> <?opt_ws> \)
                # { say "found parameter list: ", $<exp_seq>.perl }
            | \: <?ws> <exp_seq> <?opt_ws>
            |
                {
                    return ::Call(
                        invocant  => $$<term>,
                        method    => $$<ident>,
                        arguments => undef,
                        hyper     => $$<hyper>,
                    )
                }
            ]
            {
                return ::Call(
                    invocant  => $$<term>,
                    method    => $$<ident>,
                    arguments => $$<exp_seq>,
                    hyper     => $$<hyper>,
                )
            }
    | \[ <?opt_ws> <exp> <?opt_ws> \]
         { return ::Index(  obj => $$<term>, index => $$<exp> ) }   # $a[exp]
    | \{ <?opt_ws> <exp> <?opt_ws> \}
         { return ::Lookup( obj => $$<term>, index => $$<exp> ) }   # $a{exp}
    |    { return $$<term> }
    ]
}

token term {
    [ 
    | $<op> := [ \$ | \@ | \% | \? | \++ | \-- | \+ | \- | \~ ] <before <[ \( \$ ]> > <exp> 
          { return ::Apply(
            code      => 'prefix:<' ~ $$<op> ~ '>',
            arguments => [ $$<exp> ],
          ) }
    | \( <?opt_ws> <exp> <?opt_ws> \)
        { return $$<exp> }   # ( exp )
    | \{ <?opt_ws> <exp_mapping> <?opt_ws> \}
        { return ::Lit::Hash( hash => $$<exp_mapping> ) }   # { exp => exp, ... }
    | \[ <?opt_ws> <exp_seq> <?opt_ws> \]
        { return ::Lit::Array( array => $$<exp_seq> ) }   # [ exp, ... ]
    | \$ \< <ident> \>
        { return ::Lookup( 
            obj   => ::Var( sigil => '$', twigil => '', name => '/' ), 
            index => ::Val::Buf( buf => $$<ident> ) 
        ) }   # $<ident>
    | do <?opt_ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
        { return ::Do( block => $$<exp_stmts> ) }   # do { stmt; ... }
    | $<decl> := [ my | state | has ]  <?ws> <var> 
        { return ::Decl( decl => $$<decl>, var => $$<var> ) }    # my $variable
    | use <?ws> $<mod> := <full_ident>  [ - <ident> | <''> ]
        { return ::Use( mod => $$<mod> ) }
    | $<term> := <var>       # $variable
    | $<term> := <val>       # "value"
    | $<term> := <lit>       # [literal construct]
#   | $<term> := <bind>      # $lhs := $rhs
    | $<term> := <token>     # token  { regex... }
    | $<term> := <method>    # method { code... }
    | $<term> := <sub>       # sub    { code... }
    | $<term> := <control>   # Various control structures.  Does _not_ appear in binding LHS
#   | $<term> := <index>     # $obj[1, 2, 3]
#   | $<term> := <lookup>    # $obj{'1', '2', '3'}
    ]
    { return $$<term> }
}

#token index { XXX }
#token lookup { XXX }

token control {
    [ $<exp> := <return>    # return 123;
    | $<exp> := <leave>     # last; break;
    | $<exp> := <if>        # 1 ?? 2 !! 3
    | $<exp> := <when>      # when 3 { ... }
    | $<exp> := <for>       # $x.map(-> $i {...})
    | $<exp> := <while>     # while ... { ... }
    | $<exp> := <apply>     # $obj($arg1, $arg2)
 #  | $<exp> := <call>      # $obj.method($arg1, $arg2)
    ]
    { return $$<exp> }
}

token if {
    if <?ws>  <exp>  <?opt_ws>
    \{ <?opt_ws> $<body>      := <exp_stmts> <?opt_ws> \} <?opt_ws>
    else <?opt_ws> 
    \{ <?opt_ws> $<otherwise> := <exp_stmts> <?opt_ws> \}
    { return ::If( cond => $$<exp>, body => $$<body>, otherwise => $$<otherwise> ) }
}

token when {
    when <?ws> <exp_seq> <?opt_ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
    { return ::When( parameters => $$<exp_seq>, body => $$<exp_stmts> ) }
}

token for {
    for <?ws> <exp> <?opt_ws> <'->'> <?opt_ws> <var> <?ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
    { return ::For( cond => $$<exp>, topic => $$<var>, body => $$<exp_stmts> ) }
}

token while {
    while <?ws> <exp> <?ws> \{ <?opt_ws> <exp_stmts> <?opt_ws> \}
    { return ::While( cond => $$<exp>, body => $$<exp_stmts> ) }
}

token leave {
    leave
    { return ::Leave() }
}

token return {
    return <?ws> <exp>
    { return ::Return( result => $$<exp> ) }
    |
    return 
    { return ::Return( result => ::Val::Undef() ) }
}

token var {
    $<sigil>  := [ <[ \$ \% \@ \& ]> ]
    $<twigil> := [ <[ \. \! \^ \* ]> | <''> ]
    $<name>   := [ <full_ident> | <'/'> | <digit> ]
    {
        return ::Var(
            sigil  => ~$<sigil>,
            twigil => ~$<twigil>,
            name   => ~$<name>,
        )
    }
}

token val {
    [ $<exp> := <val_undef>    # undef
    # | $<exp> := <val_object>   # (not exposed to the outside)
    | $<exp> := <val_int>      # 123
    | $<exp> := <val_bit>      # True, False
    | $<exp> := <val_num>      # 123.456
    | $<exp> := <val_buf>      # "moose"
    ]
    { return $$<exp> }
}

token val_bit {
    | True>>  { return ::Val::Bit( bit => 1 ) }
    | False>> { return ::Val::Bit( bit => 0 ) }
}

token val_undef {
    undef
    { return ::Val::Undef() }
}

token val_num {  XXX { return "TODO: val_num" } }
token val_buf {
    | \" ([\\<(.)>|<-[\"]>]*)    \" { return ::Val::Buf( buf => $$0 ) }
    | \' ([\\<[\\\']>|<-[\']>]*) \' { return ::Val::Buf( buf => $$0 ) }
}

token val_int {
    \d+
    { return ::Val::Int( int => ~$/ ) }
}

token exp_stmts {
    | <exp>
        [
        |   <?opt_ws> \; <?opt_ws> <exp_stmts>
            <?opt_ws> [\; <?opt_ws>]?
            { return [ $$<exp>, array( $$<exp_stmts> ) ] }
        |   <?opt_ws> [\; <?opt_ws>]?
            { return [ $$<exp> ] }
        ]
    | { return [] }
}

token exp_seq {
    | <exp>
        # { say "exp_seq: matched <exp>" }
        [
        |   <?opt_ws> \, <?opt_ws> <exp_seq> 
            <?opt_ws> [\, <?opt_ws>]?
            { return [ $$<exp>, array( $$<exp_seq> ) ] }
        |   <?opt_ws> [\, <?opt_ws>]?
            { return [ $$<exp> ] }
        ]
    | 
        # { say "exp_seq: end of match" }
        { return [] }
}

token exp_mapping {
    |   $<key> := <exp> 
        <?opt_ws> <'=>'> <?opt_ws>
        $<value> := <exp>
        [
        |   <?opt_ws> \, <?opt_ws> <exp_mapping> 
            { return [ [ $$<key>, $$<value> ], array( $$<exp_mapping> ) ] }
        |   <?opt_ws> [\, <?opt_ws>]?
            { return [ [ $$<key>, $$<value> ] ] }
        ]
    |
        { return [] }
}

token lit {
    [ $<exp> := <lit_seq>      # (a, b, c)
    | $<exp> := <lit_array>    # [a, b, c]
    | $<exp> := <lit_hash>     # {a => x, b => y}
    | $<exp> := <lit_code>     # sub $x {...}
    | $<exp> := <lit_object>   # ::Tree(a => x, b => y);
    ]
    { return $$<exp> }
}

token lit_seq {  XXX { return "TODO: lit_seq" } }
token lit_array {  XXX { return "TODO: lit_array" } }
token lit_hash {  XXX { return "TODO: lit_hash" } }

token lit_code {
    XXX { return "TODO - Lit::Code" }
}

token lit_object {
    <'::'>
    <ident>
    \( <?opt_ws> <exp_mapping> <?opt_ws> \)
    {
        return ::Lit::Object(
            'class' => $$<ident>,
            fields => $$<exp_mapping>
        )
    }
}

token bind {
    $<parameters> := <exp>
    <?opt_ws> <':='> <?opt_ws>
    $<arguments>  := <exp>
    {
        return ::Bind(
            parameters => $$<parameters>,
            arguments  => $$<arguments>,
        )
    }
}
token call {
    $<invocant>  := <exp>
    \. $<method> := <ident> \( <?opt_ws> <exp_seq> <?opt_ws> \)
    {
        return ::Call(
            invocant  => $$<invocant>,
            method    => $$<method>,
            arguments => $$<exp_seq>,
        )
    }
}

token apply {
    <ident>
        [ \( <?opt_ws> <exp_seq> <?opt_ws> \)
        | <?ws> <exp_seq> <?opt_ws>
        ]
    {
        return ::Apply(
            code      => $$<ident>,
            arguments => $$<exp_seq>,
        )
    }
}

token token {
    # { say "parsing Token" }
    token
    [ <?ws>
      $<name> := [ <ident> ] 
    | $<name> := [ <''> ] 
    ]
    <?opt_ws> \{
        <MiniPerl6::Grammar::Regex.rule>
    \}
    {
        # say "Token was compiled into: ", ($$<MiniPerl6::Grammar::Regex.rule>).perl;
        my $source := 'method ' ~ $$<name> ~ ' ( $grammar: $str ) { ' ~
            'my $m; $m := ::Match( "str" => $str, "from" => 0, "to" => 0 ); ' ~ 
            '$m.bool( ' ~
                ($$<MiniPerl6::Grammar::Regex.rule>).emit ~
            '); ' ~
            'return $m }';
        say "Intermediate code: ", $source;
        my $ast := MiniPerl6::Grammar.term( $source );
        # say "Intermediate ast: ", $$ast.emit;
        return $$ast;
    }
}

token invocant {
    |  <var> \:    { return $$<var> }
    |  { return ::Var( 
            sigil  => '$',
            twigil => '',
            name   => 'self',
         ) 
       }
}

token sig {
        <invocant>
        <?opt_ws> 
        # TODO - exp_seq / exp_mapping == positional / named 
        <exp_seq> 
        {
            # say " invocant: ", ($$<invocant>).perl;
            # say " positional: ", ($$<exp_seq>).perl;
            return ::Sig( invocant => $$<invocant>, positional => $$<exp_seq>, named => {} );
        }
}

token method_sig {
    |   <?opt_ws> \( <?opt_ws>  <sig>  <?opt_ws>  \)
        { return $$<sig> }
    |   { return ::Sig( 
            invocant => ::Var( 
                sigil  => '$',
                twigil => '',
                name   => 'self' ), 
            positional => [], named => {} ) }
}

token method {
    method
    [  |  <?ws> $<name> := [ <ident> ] 
       |  $<name> := [ <''> ] 
    ]
    <method_sig>
    <?opt_ws> \{ <?opt_ws>  
          # { say " parsing statement list " }
          <exp_stmts> 
          # { say " got statement list ", ($$<exp_stmts>).perl } 
        <?opt_ws> 
    [   \}     | { say "*** Syntax Error in method '", $Class_name, '.', $$<name>, "' near pos=", $/.to; die "error in Block"; } ]
    {
        # say " block: ", ($$<exp_stmts>).perl;
        return ::Method( name => $$<name>, sig => $$<method_sig>, block => $$<exp_stmts> );
    }
}

token sub {
    sub
    [  |  <?ws> $<name> := [ <ident> ] 
       |  $<name> := [ <''> ] 
    ]
    <method_sig>
    <?opt_ws> \{ <?opt_ws>  
          <exp_stmts> <?opt_ws> 
    [   \}     | { say "*** Syntax Error in sub '", $$<name>, "'"; die "error in Block"; } ]
    { return ::Sub( name => $$<name>, sig => $$<method_sig>, block => $$<exp_stmts> ) }
}

}
