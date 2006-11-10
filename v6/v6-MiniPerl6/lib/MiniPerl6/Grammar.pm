use v6-alpha;

use MiniPerl6::Grammar::Regex;

grammar MiniPerl6::Grammar;

# XXX - move to v6.pm emitter
sub array($data)    { use v5; @$data; use v6; }

token comp_unit {
    <?ws>?
    class <?ws>? <ident> <?ws>? \{
        <?ws>?
        <exp_stmts>
        <?ws>?
    \}
    <?ws>?
    {
        return ::CompUnit(
            name        => $$<ident>,
            attributes  => {},
            methods     => {},
            body        => $$<exp_stmts>,
        )
    }
}

token exp {
    # { say "exp: going to match <term_meth> at ", $/.to; }
    <term_meth> 
    [
        <?ws>?
        $<op> := [ <'??'> ]
        [
          <?ws>?  <exp>
          <?ws>?  <'!!'>
          <?ws>?
          $<exp_3> := <exp>
          { return ::Op::Ternary( 
            term0 => $$<term_meth>, 
            term1 => $$<exp>, 
            term2 => $$<exp_3>,
            op    => $$<op> 
          ) }
        | { say "*** Syntax error in ternary operation" }
        ]
    |
        <?ws>?
        $<op> := [ \+ | \- | \* |/ | eq | ne | == | != | \&\& | \|\| ]
        <?ws>?
        <exp>
        { 
            # say "Op::Infix term2=", ($<exp>).perl; 
          return ::Op::Infix( 
            term0 => $$<term_meth>, 
            term1 => $$<exp>, 
            op    => $$<op> 
        ) }
    | <?ws>? <':='> <?ws>? <exp>
        { return ::Bind(parameters => $$<term_meth>, arguments => $$<exp>) }
    |   { return $$<term_meth> }
    ]
}

token term_meth {
    <term>
    [ \. <ident>
          [
            [ \( 
                # { say "testing exp_seq at ", $/.to }
                <?ws>? <exp_seq> <?ws>? \)
                # { say "found parameter list: ", $<exp_seq>.perl }
            | \: <?ws> <exp_seq> <?ws>?
            ]
            {
                return ::Call(
                    invocant  => $$<term>,
                    method    => $$<ident>,
                    arguments => $$<exp_seq>,
                )
            }
          |
            {
                return ::Call(
                    invocant  => $$<term>,
                    method    => $$<ident>,
                    arguments => undef,
                )
            }
          ]
    |    { return $$<term> }
    ]
}

token term {
    [ 
    | \( <?ws>? <exp> <?ws>? \)
      { return $$<exp> }   # ( exp )
    | $<decl> := [ my | state ]
      <?ws> <var> 
      { return ::Decl( decl => $$<decl>, var => $$<var> ) }    # my $variable
    | $<term> := <var>       # $variable
    | $<term> := <val>       # "value"
    | $<term> := <lit>       # [literal construct]
#   | $<term> := <bind>      # $lhs := $rhs
    | $<term> := <token>     # token { regex... }
    | $<term> := <method>    # method { code... }
    | $<term> := <control>   # Various control structures.  Does _not_ appear in binding LHS
    | $<term> := <index>     # $obj[1, 2, 3]
    | $<term> := <lookup>    # $obj{'1', '2', '3'}
    ]
    { return $$<term> }
}

token index { XXX }
token lookup { XXX }

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
    if <?ws>  <exp>  <?ws>?
    \{ <?ws>? $<body>      := <exp_stmts> <?ws>? \} <?ws>?
    else <?ws>? 
    \{ <?ws>? $<otherwise> := <exp_stmts> <?ws>? \}
    { return ::If( cond => $$<exp>, body => $$<body>, otherwise => $$<otherwise> ) }
}

token when {
    when <?ws> <exp_seq> <?ws>? \{ <?ws>? <exp_stmts> <?ws>? \}
    { return ::When( parameters => $$<exp_seq>, body => $$<exp_stmts> ) }
}

token for {
    for <?ws> <exp> <?ws>? <'->'> <?ws>? <var> <?ws> \{ <?ws>? <exp_stmts> <?ws>? \}
    { return ::For( cond => $$<exp>, topic => $$<var>, body => $$<exp_stmts> ) }
}

token while {
    while <?ws> <exp> <?ws> \{ <?ws>? <exp_stmts> <?ws>? \}
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
    $<twigil> := [ <[ \. \! \^ ]> | <''> ]
    <ident>
    {
        return ::Var(
            sigil  => ~$<sigil>,
            twigil => ~$<twigil>,
            name   => ~$<ident>,
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
    | \" ([\\<(.)>|<-[\"]>]+) \" { return ::Val::Buf( buf => $$0 ) }
    | \' ([\\<[\\\']>|<-[\']>]+) \' { return ::Val::Buf( buf => $$0 ) }
}

token val_int {
    \d+
    { return ::Val::Int( int => ~$/ ) }
}

token exp_stmts {
    | <exp>
        [
        |   <?ws>? \; <?ws>? <exp_stmts>
            <?ws>? [\; <?ws>?]?
            { return [ $$<exp>, array( $$<exp_stmts> ) ] }
        |   <?ws>? [\; <?ws>?]?
            { return [ $$<exp> ] }
        ]
    | { return [] }
}

token exp_seq {
    | <exp>
        # { say "exp_seq: matched <exp>" }
        [
        |   <?ws>? \, <?ws>? <exp_seq> 
            <?ws>? [\, <?ws>?]?
            { return [ $$<exp>, array( $$<exp_seq> ) ] }
        |   <?ws>? [\, <?ws>?]?
            { return [ $$<exp> ] }
        ]
    | 
        # { say "exp_seq: end of match" }
        { return [] }
}

token exp_mapping {
    $<key> := <exp> 
    <?ws>? <'=>'> <?ws>?
    $<value> := <exp>
    [
    |   <?ws>? \, <?ws>? <exp_mapping> 
        { return [ [ $$<key>, $$<value> ], array( $$<exp_mapping> ) ] }
    |   <?ws>? [\, <?ws>?]?
        { return [ [ $$<key>, $$<value> ] ] }
    ]
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
    \( <?ws>? <exp_mapping> <?ws>? \)
    {
        return ::Lit::Object(
            'class' => $$<ident>,
            fields => $$<exp_mapping>
        )
    }
}

token bind {
    $<parameters> := <exp>
    <?ws>? <':='> <?ws>?
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
    \. $<method> := <ident> \( <?ws>? <exp_seq> <?ws>? \)
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
        [ \( <?ws>? <exp_seq> <?ws>? \)
        | <?ws> <exp_seq> <?ws>?
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
    <?ws>? \{
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
        say "Intermediate code: $source";
        my $ast := MiniPerl6::Grammar.term( $source );
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
        <?ws>? 
        # TODO - exp_seq / exp_mapping == positional / named 
        <exp_seq> 
        {
            # say " invocant: ", ($$<invocant>).perl;
            # say " positional: ", ($$<exp_seq>).perl;
            return ::Sig( invocant => $$<invocant>, positional => $$<exp_seq>, named => {} );
        }
}

token method {
    method
        { 
            say "Parsing method:";
        }
    [  |  <?ws> $<name> := [ <ident> ] 
       |  $<name> := [ <''> ] 
    ]
    <?ws>? \( <?ws>?  <sig>  <?ws>?  \)
        # { 
        #    say " name: ", ($$<name>).perl;
        #    say " params: ", ($$<sig>).perl;
        # } 
    <?ws>? \{ <?ws>?  
          # { say " parsing statement list " }
          <exp_stmts> 
          # { say " got statement list ", ($$<exp_stmts>).perl } 
        <?ws>? 
    [   \}     | { say "*** error in Block"; die "error in Block"; } ]
    {
        # say " block: ", ($$<exp_stmts>).perl;
        return ::Method( name => $$<name>, sig => $$<sig>, block => $$<exp_stmts> );
    }
}
