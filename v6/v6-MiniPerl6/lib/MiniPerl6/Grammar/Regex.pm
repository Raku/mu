
use v6-alpha;

grammar MiniPerl6::Grammar::Regex;

my %rule_terms;
my %variables;

token pod_begin {
    |   \n =end \N*
    |   . \N* <?pod_begin>
}

token pod_other {
    |   \n =cut \N*
    |   . \N* <?pod_other>
}

token ws {
    [
    |    \# \N*
    |    \n [ = [
            |  begin <?ws> END \N* .*
            |  begin  <?pod_begin>
            |  kwid   <?pod_other>
            |  pod    <?pod_other>
            |  for    <?pod_other>
            |  head1  <?pod_other>
            ]?
            ]?
    |    \s
    ]+
}

# regex ident can start with a number
token ident {
    [ <?alnum> | _ | <'::'> ]+
}

token literal {
    [ 
    |  \\ .
    |  <-[ \' ]> 
    ]*
}

token metasyntax {
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code>        \}
    |  \<  <?metasyntax>  \>
    |  <-[ \> ]> 
    ]+ 
    { return { metasyntax => $$/ ,} }
}

token char_range {
    [ 
    |  \\ .
    |  <-[ \] ]> 
    ]+ 
}

token char_class {
    |  <?ident>
    |  \[  <?char_range>  \]
}

# XXX - not needed
token string_code {
    # bootstrap "code"
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code> \}
    |  <-[ \} ]> 
    ]+ 
}

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    # XXX - call MiniPerl6 "Statement List"
    <?string_code>
    { return '{' ~ $/ ~ '}' }
}

token named_capture_body {
    | \(  <rule>        \)  { return { capturing_group => $$<rule> ,} } 
    | \[  <rule>        \]  { return $$<rule> } 
    | \<  <metasyntax>  \>  { return $$<metasyntax> } 
    | { die "invalid alias syntax" }
}

%variables := (

    '$<' => token {
        <ident> \> 
        { return '$/{' ~ "'" ~ $<ident> ~ "'" ~ '}' }
    },
    '$' => token { 
        <?digit>+
        { return '$/[' ~ $/ ~ ']' }
    |
        (\^?)
        ([ <?alnum> | _ | \: \: ]+)
        {
            # $a $^a
            return Var({ 
                    sigil  => '$',
                    twigil => ~$0,
                    name   => ~$1,
                   }),
        }
    },
    '@' => token { 
        <?digit>+
        # TODO
        { return { match_variable => '@' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        # TODO
        { return { variable => '@' ~ $/ ,} }
    },
    '%' => token { 
        <?digit>+
        # TODO
        { return { match_variable => '%' ~ $/ ,} }
    |        
        (\^?)
        ([ <?alnum> | _ | \: \: ]+)
        {
            # %a %^a
            return Var({ 
                    sigil  => '%',
                    twigil => ~$0,
                    name   => ~$1,
                   }),
        }
    },

); # /%variables
    

%rule_terms := (

    '(' => token {
        <rule> \)
        { return { Rul::Capture({ :$$<rule> }) } }
    },
    '<(' => token {
        <rule>  <')>'>
        { return { Rul::CaptureResult({ :$$<rule> }) } }
    },
    '<after' => token {
        <?ws> <rule> \> 
        { return { Rul::After({ :$$<rule> }) } }
    },
    '<before' => token {
        <?ws> <rule> \> 
        { return { Rul::Before({ :$$<rule> }) } }
    },
    '<!before' => token {
        <?ws> <rule> \> 
        # TODO
        { return { not_before => :$$<rule>, } }
    },
    '<!' => token {
        # TODO
        <metasyntax> \> 
        { return { negate  => $$<metasyntax>, } }
    },
    '<+' => token {
        # TODO
        $<c0> := <char_class>  \> 
        { return { metasyntax => ~ $<c0> } }
    },
    '<-' => token {
        # TODO
        <char_class> \>
        { return { metasyntax => '-' ~ $<char_class> } }
    },
    '<' => token { 
        |  <%MiniPerl6::Grammar::Regex::variables>
            { 
                return { Rul::InterpolateVar({ var => $$<MiniPerl6::Grammar::Regex::variables> }) }
            }
        |
            # TODO
            <metasyntax>  \>
            { return $$<metasyntax> }
    },
    '{' => token { 
        <parsed_code>  \}
        { return { Rul::Block({ closure => $$<parsed_code> }) } }
    },
    '\\' => token {  
        | [ x | X | o | O ] \d+
          #  \x0021    \X0021
          { return { Rul::SpecialChar({ char => '\\' ~ $/ }) } }
        | ( x | X | o | O ) \[ (\d+) \]
          #  \x[0021]  \X[0021]
          { return { Rul::SpecialChar({ char => '\\' ~ $0 ~ $1 }) } }
        | .
          #  \e  \E
          { return { Rul::SpecialChar({ char => '\\' ~ $/ }) } }
    },
    '.' => token { 
        { return { Rul::Dot({ dot => 1 }) } }
    },
    '[' => token { 
        <rule> \] 
        { return $$<rule> }
    },
    ':::' => token { { return { colon => ':::' ,} } },
    ':?'  => token { { return { colon => ':?' ,} } },
    ':+'  => token { { return { colon => ':+' ,} } },
    '::'  => token { { return { colon => '::' ,} } },
    ':'   => token { { return { colon => ':'  ,} } },
    '$$'  => token { { return { colon => '$$' ,} } },
    '$'   => token { { return { colon => '$'  ,} } },
    '^^'  => token { { return { colon => '^^' ,} } },
    '^'   => token { { return { colon => '^'  ,} } },
    
    '>>'  => token { { return { colon => '>>' ,} } },
    '»'   => token { { return { colon => '>>' ,} } },

    '<<'  => token { { return { colon => '<<' ,} } },
    '«'   => token { { return { colon => '<<' ,} } },

    ':i'  => token { 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', :$$<rule>, } } },
    ':ignorecase'  => token { 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', :$$<rule>, } } },
    ':s'  => token { 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   :$$<rule>, } } },
    ':sigspace'    => token { 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   :$$<rule>, } } },
    ':P5' => token { 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  :$$<rule>, } } },
    ':Perl5'       => token { 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  :$$<rule>, } } },
    ':bytes'       => token { 
        <?ws> <rule> 
        { return { modifier => 'bytes',  :$$<rule>, } } },
    ':codes'       => token { 
        <?ws> <rule> 
        { return { modifier => 'codes',  :$$<rule>, } } },
    ':graphs'      => token { 
        <?ws> <rule> 
        { return { modifier => 'graphs', :$$<rule>, } } },
    ':langs'       => token { 
        <?ws> <rule> 
        { return { modifier => 'langs',  :$$<rule>, } } },

); # /%rule_terms
    
token term {
    |  <%Pugs::Grammar::Rule::variables>
       [  <?ws>? <':='> <?ws>? <named_capture_body>
          { 
            return { named_capture => {
                rule =>  $$<named_capture_body>,
                ident => $$<Pugs::Grammar::Rule::variables>,
            }, }; 
          }
       |
          { 
            return $$<Pugs::Grammar::Rule::variables> 
          }
       ]
    |  <%Pugs::Grammar::Rule::rule_terms>
        { 
            #print "term: ", Dumper( $_[0]->data );
            return $$<Pugs::Grammar::Rule::rule_terms> 
        }
    |  <-[ \] \} \) \> \: \? \+ \* \| \& ]>    # TODO - <...>* - optimize!
        { return { Rul::Constant({ constant => $$/ }) } }
}

token quant {
    |   <'**'> <?ws>? \{  <parsed_code>  \}
        { return { closure => $$<parsed_code> ,} }
    |   <[  \? \* \+  ]>?
}

token quantifier {
    $<ws1>   := (<?ws>?)
    <!before  <[   \} \] \)   ]> >
    <term> 
    $<ws2>   := (<?ws>?)
    <quant>
    $<greedy> := (<[  \? \+  ]>?)
    $<ws3>   := (<?ws>?)
    { return { Rul::Quantifier({
            term    => $$/{'term'},
            quant   => $$/{'quant'},
            greedy  => $$/{'greedy'},
            ws1     => $$/{'ws1'},
            ws2     => $$/{'ws2'},
            ws3     => $$/{'ws3'},
        }) }
    }
}

token concat {
    <quantifier>+ 
    { return { Rul::Concat({ ...  }) }
    }
}

token conjunctive {
    [ <?ws>? \& ]?
    
    $<first_concat> := <concat>
    [
        \&  <concat> 
    ]*
    
    {             
        ...
    }
}

token rule {
    [ <?ws>? \| ]?
    
    $<first_conjunctive> := <conjunctive>
    [
        \|  <conjunctive> 
    ]*
    
    {             
        ...
    }
}
