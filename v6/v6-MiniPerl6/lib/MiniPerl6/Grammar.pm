use v6-alpha;

grammar MiniPerl6::Grammar;

sub CompUnit($data)  { use v5; bless $data, 'CompUnit';  use v6; }
sub Var($data)       { use v5; bless $data, 'Var';       use v6; }
sub Apply($data)     { use v5; bless $data, 'Apply';     use v6; }
sub Call($data)      { use v5; bless $data, 'Call';      use v6; }
sub Bind($data)      { use v5; bless $data, 'Bind';      use v6; }
sub Return($data)    { use v5; bless $data, 'Return';    use v6; }
sub While($data)     { use v5; bless $data, 'While';     use v6; }
sub For($data)       { use v5; bless $data, 'For';       use v6; }
sub When($data)      { use v5; bless $data, 'When';      use v6; }
sub If($data)        { use v5; bless $data, 'If';        use v6; }

sub Lit::Object($data) { use v5; bless $data, 'Lit::Object'; use v6; }

sub Val::Undef($data)  { use v5; bless $data, 'Val::Undef';  use v6; }
sub Val::Int($data)    { use v5; bless $data, 'Val::Int';    use v6; }
sub Val::Num($data)    { use v5; bless $data, 'Val::Num';    use v6; }

# XXX - move to v6.pm emitter
sub array($data)    { use v5; @$data; use v6; }

token comp_unit {
    <?ws>?
    class <?ws>? <ident> \{
        <?ws>?
    \}
    <?ws>?
    {
        return CompUnit({
            name => $<ident>,
            attributes => {},
            methods => {},
            body => {},
        })
    }
}

token exp {
    [ $<exp> := <var>       # $variable
    | $<exp> := <val>       # "value"
    | $<exp> := <lit>       # [literal construct]
    | $<exp> := <bind>      # $lhs := $rhs
    | $<exp> := <index>     # $obj[1, 2, 3]
    | $<exp> := <lookup>    # $obj{'1', '2', '3'}
    | $<exp> := <control>   # Various control structures.  Does _not_ appear in binding LHS
    ]
    { return $$<exp> }
}

token control {
    [ $<exp> := <call>      # $obj.method($arg1, $arg2)
    | $<exp> := <apply>     # $obj($arg1, $arg2)
    | $<exp> := <return>    # return 123;
    | $<exp> := <leave>     # last; break;
    | $<exp> := <if>        # 1 ?? 2 !! 3
    | $<exp> := <when>      # when 3 { ... }
    | $<exp> := <for>       # $x.map(-> $i {...})
    | $<exp> := <while>     # while ... { ... }
    ]
    { return $$<exp> }
}

token if {
    if <?ws>  $<cond>      := <exp>     <?ws>?
    \{ <?ws>? $<body>      := <exp_seq> <?ws>? \} <?ws>?
    else <?ws>? 
    \{ <?ws>? $<otherwise> := <exp_seq> <?ws>? \}
    { return If({ :$$<cond>, :$$<body>, :$$<otherwise> }) }
}

token when {
    when <?ws> $<parameters> := <exp_seq> <?ws>? \{ <?ws>? $<body> := <exp_seq> <?ws>? \}
    { return When({ :$$<parameters>, :$$<body> }) }
}

token for {
    for <?ws> <exp> <?ws>? <'->'> <?ws>? <var> <?ws> \{ <?ws>? <exp_seq> <?ws>? \}
    { return For({ cond => $$<exp>, topic => $$<var>, body => $$<exp_seq> }) }
}

token while {
    while <?ws> <exp> <?ws> \{ <?ws>? <exp_seq> <?ws>? \}
    { return While({ cond => $$<exp>, body => $$<exp_seq> }) }
}

token return {
    return <?ws> <exp>
    { return Return({ result => $$<exp> }) }
    |
    return 
    { return Return({ result => Val::Undef({}) }) }
}

token var {
    $<sigil>  := [ <[ \$ \% \@ \& ]> ]
    $<twigil> := [ <[ \. \! \^ ]> | <''> ]
    <ident>
    {
        return Var({
            sigil  => ~$<sigil>,
            twigil => ~$<twigil>,
            name   => ~$<ident>,
        })
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

token val_undef {
    undef
    { return Val::Undef({ undef => 1 }) }
}

token val_int {
    \d+
    { return Val::Int( { int => ~$/ } ) }
}

token exp_seq {
    <exp>
    [
    |   <?ws>? \, <?ws>? <exp_seq> 
        { return [ $$<exp>, array( $$<exp_seq> ) ] }
    |   { return [ $$<exp> ] }
    ]
}

token exp_mapping {
    $<key> := <exp> 
    <?ws>? <'=>'> <?ws>?
    $<value> := <exp>
    [
    |   <?ws>? \, <?ws>? <exp_mapping> 
        { return [ [ $$<key>, $$<value> ], array( $$<exp_mapping> ) ] }
    |   { return [ [ $$<key>, $$<value> ] ] }
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

token lit_code {
    { die "TODO - Lit::Code" }
}

token lit_object {
    <'::'>
    $<class> := <ident>
    \( <?ws>? $<fields> := <exp_mapping> <?ws>? \)
    {
        return Lit::Object({
            :$$<class>,
            :$$<fields>,
        })
    }
}

token bind {
    $<parameters> := <exp>
    <?ws>? <':='> <?ws>?
    $<arguments>  := <exp>
    {
        return Bind({
            :$$<parameters>,
            :$$<arguments>,
        })
    }
}
token call {
    $<invocant>  := <exp>
    \. $<method> := <ident> \( <?ws>? <exp_seq> <?ws>? \)
    {
        return Call({
            :$$<invocant>,
            :$$<method>,
            arguments => $$<exp_seq>,
        })
    }
}
token apply {
    <ident> \( <?ws>? <exp_seq> <?ws>? \)
    {
        return Apply({
            code      => $$<ident>,
            arguments => $$<exp_seq>,
        })
    }
}

=begin END

# A program is a sequence of compilation units.
subset Program of (Seq of CompUnit);

# A compilation unit is a named class.
class CompUnit {
    has $.class         is Type;                    # class Name;
    has %.attributes    is Mapping of Type;         # has $.attr is Type;
    has %.methods       is Mapping of Lit::Code;    # method foo { ... }
    has $.body          is Lit::Code;               # body of code
}

subset Exp of
    ( Var       # $variable
    | Val       # "value"
    | Lit       # [literal construct]
    | Bind      # $lhs := $rhs
    | Index     # $obj[1, 2, 3]
    | Lookup    # $obj{'1', '2', '3'}
    | Control   # Various control structures.  Does _not_ appear in binding LHS
    );

subset Control of
    ( Call      # $obj.method($arg1, $arg2)
    | Apply     # $obj($arg1, $arg2)
    | Return    # return 123;
    | Leave     # last; break;
    | If        # 1 ?? 2 !! 3
    | When      # when 3 { ... }
    | For       # $x.map(-> $i {...})
    | While     # while ... { ... }
    );

subset ID of Str;
subset Type of Str;

enum Sigil ('$', '%', '@', '&');
enum Twigil ('', '.', '!', '^');

class If {
    has $.cond          is Exp;
    has @.body          is Seq of Exp;
    has @.otherwise     is Seq of Exp;
}

class When {
    has @.parameters    is Seq of Exp;
    has @.body          is Seq of Exp;
}

class For {
    has $.cond          is Exp;
    has $.topic         is Var;
    has @.body          is Seq of Exp;
}

class While {
    has $.cond          is Exp;
    has @.body          is Seq of Exp;
}

class Return {
    has $.result    is Exp;
}

class Var {
    has $.sigil     is Sigil;
    has $.twigil    is Twigil;
    has $.name      is ID;
}

subset Val of
    ( Val::Undef    # undef
    | Val::Object   # (not exposed to the outside)
    | Val::int      # 123
    | Val::bit      # True, False
    | Val::num      # 123.456
    | Val::buf      # "moose"
    );

class Val::Object {
    has $.class         is Type;
    has %.fields        is Mapping of Val;
}

subset Lit of
    ( Lit::Seq      # (a, b, c)
    | Lit::Array    # [a, b, c]
    | Lit::Hash     # {a => x, b => y}
    | Lit::Code     # sub $x {...}
    | Lit::Object   # ::Tree(a => x, b => y);
    );

class Lit::Code {
    has %.pad           is Mapping of Type; # All my/state/parameter variables
    has %.state         is Mapping of Exp;  # State initializers, run upon first entry 
    has @.parameters    is Seq of Exp;      # Signature
    has @.body          is Seq of Exp;      # Code body 
}

class Lit::Object {
    has $.class         is Type;            # Class name
    has %.fields        is Mapping of Exp;  # Field initializers
}

class Bind {
    has @.parameters    is Exp;             # Signature
    has @.arguments     is Exp;             # Capture
}

class Call {
    has $.invocant  is Exp;                 # $obj
    has $.method    is ID;                  # .method
    has @.arguments is Seq of Exp;          # ($args)
}

class Apply {
    has $.code      is Exp;                 # &sub
    has @.arguments is Seq of Exp;          # ($args)
}

