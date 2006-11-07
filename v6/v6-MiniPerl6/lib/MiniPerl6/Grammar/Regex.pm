
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

%variables{'$<'} := token {
        <ident> \> 
        { return '$/{' ~ "'" ~ $<ident> ~ "'" ~ '}' }
};
%variables{'$'} := token {
    |
        # TODO
        <?digit>+
        { return '$/[' ~ $/ ~ ']' }
    |
        (\^?)
        ([ <?alnum> | _ | \: \: ]+)
        {
            # $a $^a
            return Rul::Var( 
                    sigil  => '$',
                    twigil => ~$0,
                    name   => ~$1
                   )
        }
};
%variables{'@'} := token {
        <?digit>+
        # TODO
        { return { match_variable => '@' ~ $/ ,} }
    |
        \^?
        [ <?alnum> | _ | \: \: ]+
        # TODO
        { return { variable => '@' ~ $/ ,} }
};
%variables{'%'} := token {
    |
    # { say "matching hash-digit" }
        <?digit>+
        # TODO
        { return { match_variable => '%' ~ $/ ,} }
    |        
    # { say "matching hash-ident" }
        (\^?)
        ([ <?alnum> | _ | \: \: ]+)
        {
            # %a %^a
            # say "found hash-ident";
            return Rul::Var(
                    'sigil'  => '%',
                    'twigil' => ~$0,
                    'name'   => ~$1
                   )
        }
};

%rule_terms{'('} := token {
        <rule> \)
        { return Rul::Capture({ :$$<rule> }) }
};
%rule_terms{'<('} := token {
        <rule>  <')>'>
        { return Rul::CaptureResult({ :$$<rule> }) }
};
%rule_terms{'<after'} := token {
        <?ws> <rule> \> 
        { return Rul::After({ :$$<rule> }) }
};
%rule_terms{'<before'} := token {
        <?ws> <rule> \> 
        { return Rul::Before({ :$$<rule> }) }
};
%rule_terms{'<!before'} := token {
        <?ws> <rule> \> 
        # TODO
        { return { not_before => :$$<rule>, } }
};
%rule_terms{'<!'} := token {
        # TODO
        <metasyntax> \> 
        { return { negate  => $$<metasyntax>, } }
};
%rule_terms{'<+'} := token {
        # TODO
        $<c0> := <char_class>  \> 
        { return { metasyntax => ~ $<c0> } }
};
%rule_terms{'<-'} := token {
        # TODO
        <char_class> \>
        { return { metasyntax => '-' ~ $<char_class> } }
};
%rule_terms{"<'"} := token {
    <literal> \' \>
    { return Rul::Constant( constant => $$<literal> ) }
}
%rule_terms{'<'} := token { 
        |  
        # { say "matching < ..." }
           <%variables>  # \>
            {
                # say "found < hash-variable >";
                return Rul::InterpolateVar( var => $$<variables> )
            }
        |
            # TODO
            <metasyntax>  \>
            { return Rul::Subrule( $$<metasyntax> ) }
};
%rule_terms{'{'} := token { 
        <parsed_code>  \}
        { return Rul::Block({ closure => $$<parsed_code> }) }
};
%rule_terms{'\\'} := token {  
        | [ x | X ] <[ 0..9 a..f A..F ]]>+
          #  \x0021    \X0021
          { return Rul::SpecialChar({ char => '\\' ~ $/ }) }
        | [ o | O ] <[ 0..7 ]>+
          #  \x0021    \X0021
          { return Rul::SpecialChar({ char => '\\' ~ $/ }) }
        | ( x | X | o | O ) \[ (<-[ \] ]>*) \]
          #  \x[0021]  \X[0021]
          { return Rul::SpecialChar({ char => '\\' ~ $0 ~ $1 }) }
        | .
          #  \e  \E
          { return Rul::SpecialChar({ char => $$/ }) }
};
%rule_terms{'.'} := token { 
        { return Rul::Dot({ dot => 1 }) }
};
%rule_terms{'['} := token { 
        <rule> \] 
        { return $$<rule> }
};
%rule_terms{':::'} := token { { return { colon => ':::' ,} } },
%rule_terms{':?'} := token { { return { colon => ':?' ,} } };
%rule_terms{':+'} := token { { return { colon => ':+' ,} } };
%rule_terms{'::'} := token { { return { colon => '::' ,} } };
%rule_terms{':'} := token { { return { colon => ':'  ,} } };
%rule_terms{'$$'} := token { { return { colon => '$$' ,} } };
%rule_terms{'$'} := token { { return { colon => '$'  ,} } };
%rule_terms{'^^'} := token { { return { colon => '^^' ,} } };
%rule_terms{'^'} := token { { return { colon => '^'  ,} } };
    
%rule_terms{'>>'} := token { { return { colon => '>>' ,} } };
%rule_terms{'»'} := token { { return { colon => '>>' ,} } };

%rule_terms{'<<'} := token { { return { colon => '<<' ,} } };
%rule_terms{'«'} := token { { return { colon => '<<' ,} } };

%rule_terms{':i'} := token { 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', :$$<rule>, } } };
%rule_terms{':ignorecase'} := token { 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', :$$<rule>, } } };
%rule_terms{':s'} := token { 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   :$$<rule>, } } };
%rule_terms{':sigspace'} := token { 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   :$$<rule>, } } };
%rule_terms{':P5'} := token { 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  :$$<rule>, } } };
%rule_terms{':Perl5'} := token { 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  :$$<rule>, } } };
%rule_terms{':bytes'} := token { 
        <?ws> <rule> 
        { return { modifier => 'bytes',  :$$<rule>, } } };
%rule_terms{':codes'} := token { 
        <?ws> <rule> 
        { return { modifier => 'codes',  :$$<rule>, } } };
%rule_terms{':graphs'} := token { 
        <?ws> <rule> 
        { return { modifier => 'graphs', :$$<rule>, } } };
%rule_terms{':langs'} := token { 
        <?ws> <rule> 
        { return { modifier => 'langs',  :$$<rule>, } } };

token term {
    |  
       # { say "matching variables" } 
       <%variables>
       [  <?ws>? <':='> <?ws>? <named_capture_body>
          { 
            return { named_capture => {
                rule =>  $$<named_capture_body>,
                ident => $$<variables>,
            }, }; 
          }
       |
          { 
            return $$<variables>,
          }
       ]
    | 
        # { say "matching terms"; }
        <%rule_terms>
        { 
            #print "term: ", Dumper( $_[0]->data );
            return $$<rule_terms> 
        }
    |  <-[ \] \} \) \> \: \? \+ \* \| \& \/ ]>    # TODO - <...>* - optimize!
        { return Rul::Constant({ constant => $$/ }) }
}

token quant {
    |   <'**'> <?ws>? \{  <parsed_code>  \}
        { return { closure => $$<parsed_code> ,} }
    |   <[  \? \* \+  ]>
}

token quantifier {
    $<ws1>   := (<?ws>?)
    <!before  <[   \} \] \)   ]> >
    <term> 
    $<ws2>   := (<?ws>?)
    [
    <quant>
    $<greedy> := (<[  \? \+  ]>?)
    $<ws3>   := (<?ws>?)
    { return Rul::Quantifier({
            term    => $$/{'term'},
            quant   => $$/{'quant'},
            greedy  => $$/{'greedy'},
            ws1     => $$/{'ws1'},
            ws2     => $$/{'ws2'},
            ws3     => $$/{'ws3'},
        })
    }
    |
        { return $$<term> }
    ]
}

token concat_list {
    $<q1> := <quantifier>
    [
        $<q2> := <concat_list> 
        { return [ $$<q1>, @($$<q2>) ] }
    |
        { return [ $$<q1> ] }
    ]
    |
        { return [] }
}
token concat {
    <concat_list>
    { return Rul::Concat( concat => $$<concat_list> ) }
}

token or_list {
    $<q1> := <concat>
    [
        <'|'>
        $<q2> := <or_list> 
        { return [ $$<q1>, @($$<q2>) ] }
    |
        { return [ $$<q1> ] }
    ]
    |
        { return [] }
}
token rule {
    [ <'|'> | <''> ]
    # { say "trying M::G::Rule on ", $s }
    <or_list>
    { 
        # say "found Rule";
        return Rul::Or( 'or' => $$<or_list> ) 
    }
}

=pod 

# -- this depends on changing the specs
#token concat {
#    <quantifier>+
#    { return Rul::Concat( concat => $<quantifier> ) }
#}

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

=cut

