
use v6-alpha;

# Pure-Perl6 implementation of the 'Rule' syntax 
# author: Flavio S. Glock - fglock@gmail.com

# This is for demonstation only, it currently can't be run by 
# Pugs::Compiler::Rule runtime

grammar Pugs::Grammar::Rule;

our %rule_terms;
our %variables;

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

token string_code {
    # bootstrap "code"
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code>        \}
    |  <-[ \} ]> 
    ]+ 
}

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    <?string_code>
    { return '{' ~ $/ ~ '}' }
}

token named_capture_body {
    | \(  <rule>        \)  { return { capturing_group => $$<rule> ,} } 
    | \[  <rule>        \]  { return $$<rule> } 
    | \<  <metasyntax>  \>  { return $$<metasyntax> } 
    | { die "invalid alias syntax" }
}

%variables = (

    '$<' => token {
        <ident> \> 
        { return { match_variable => '$' ~ $/<ident> ,} }
    },
    '$' => token { 
        <?digit>+
        { return { match_variable => '$' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        { return { variable => '$' ~ $/ ,} }
    },
    '@' => token { 
        <?digit>+
        { return { match_variable => '@' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        { return { variable => '@' ~ $/ ,} }
    },
    '%' => token { 
        <?digit>+
        { return { match_variable => '%' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        { return { variable => '%' ~ $/ ,} }
    },

); # /%variables
    

%rule_terms = (

    '(' => token {
        <rule> \)
        { return { capturing_group => $$<rule> ,} }
    },
    '<(' => token {
        <rule>  <')>'>
        { return { capture_as_result => $$<rule> ,} }
    },
    '<after' => token {
        <?ws> <rule> \> 
        { return { after => :$$<rule>, } }
    },
    '<before' => token {
        <?ws> <rule> \> 
        { return { before => :$$<rule>, } }
    },
    '<!before' => token {
        <?ws> <rule> \> 
        { return { not_before => :$$<rule>, } }
    },
    '<!' => token {
        <metasyntax> \> 
        { return { negate  => :$$<metasyntax>, } }
    },
    '<' => token { 
        <metasyntax>  \>
        { return $$<metasyntax> }
    },
    '{' => token { 
        <parsed_code>  \}
        { return { closure => $$<parsed_code> ,} }
    },
    '\\' => token {  
        .
        { return { special_char => '\\' ~ $/ , } } 
    },
    '.' => token { 
        { return { 'dot' => 1 ,} }
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

    ':i'           => token { { return { modifier => 'ignorecase'  ,} } },
    ':ignorecase'  => token { { return { modifier => 'ignorecase'  ,} } },
    ':s'           => token { { return { modifier => 'sigspace'  ,} } },
    ':sigspace'    => token { { return { modifier => 'sigspace'  ,} } },
    ':P5'          => token { { return { modifier => 'Perl5'  ,} } },
    ':Perl5'       => token { { return { modifier => 'Perl5'  ,} } },
    ':bytes'       => token { { return { modifier => 'bytes'  ,} } },
    ':codes'       => token { { return { modifier => 'codes'  ,} } },
    ':graphs'      => token { { return { modifier => 'graphs' ,} } },
    ':langs'       => token { { return { modifier => 'langs'  ,} } },

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
    |  <-[ \] \} \) \> \: \? \+ \* \| \& ]> 
        { 
            #print "constant: ", Dumper( $_[0]->data );
            return { 'constant' => $$/ ,} 
        }
}

token quant {
    |   <'**'> <?ws>? \{  <parsed_code>  \}
        { return { closure => $$<parsed_code> ,} }
    |   <[  \? \* \+  ]>?
}

token quantifier {
    $<ws1>   := (<?ws>?)
    <!before  <[   \} \] \) \>   ]> >
    <term> 
    $<ws2>   := (<?ws>?)
    <quant>
    $<greedy> := (<[  \? \+  ]>?)
    $<ws3>   := (<?ws>?)
    { return { 
        quant => { 
            term    => $$/{'term'},
            quant   => $$/{'quant'},
            greedy  => $$/{'greedy'},
            ws1     => $$/{'ws1'},
            ws2     => $$/{'ws2'},
            ws3     => $$/{'ws3'},
        } }
    }
}

token concat {
    <quantifier>+ 
    {             
        my $a = $<quantifier>.map( {  $$_  } );
        return { concat => $a ,}  if $a.elems > 1;
        return $a[0];
    }
}

token rule {
    [ <?ws>? \| ]?
    
    <concat>
    [
        \|  <concat> 
    ]*
    
    {             
        my $a = $<concat>.map( {  $$_  } );
        return { alt => $a ,}  if $a.elems > 1;
        return $a[0];
    }
}
