
use v6-alpha;

grammar MiniPerl6::Grammar::Regex {

my %rule_terms;

token ws {  <?MiniPerl6::Grammar.ws>  };

token ident {  <?MiniPerl6::Grammar.full_ident> | <digit> };

token literal {
    |  \\ .        <literal>
    |  <-[ \' ]>   <literal>
    |  <''>
};

token metasyntax {
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code>        \}
    |  \<  <?metasyntax>  \>
    |  <-[ \> ]> 
    ]
    [ <metasyntax> | <''> ]
};

token char_range {
    [ 
    |  \\ .
    |  <-[ \] ]> 
    ]
    [ <char_range> | <''> ]
};

token char_class {
    |  <?ident>
    |  \[  <?char_range>  \]
};

# XXX - not needed
token string_code {
    # bootstrap "code"
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code> \}
    |  <-[ \} ]> 
    ]
    [ <string_code> | <''> ]
};

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    # XXX - call MiniPerl6 "Statement List"
    <?string_code>
    { return '{' ~ $/ ~ '}' }
};

token named_capture_body {
    | \(  <rule>        \)  { return { capturing_group => $$<rule> ,} } 
    | \[  <rule>        \]  { return $$<rule> } 
    | \<  <metasyntax>  \>  
            { return ::Rul::Subrule( metasyntax => $$<metasyntax> ) }
    | { die "invalid alias syntax" }
};

token variables {
    |
        <'$<'>
        <ident> \> 
        { return '$/{' ~ "'" ~ $<ident> ~ "'" ~ '}' }
    |
        # TODO
        <MiniPerl6::Grammar.sigil> 
        <MiniPerl6::Grammar.digits>
        { return $<MiniPerl6::Grammar.sigil> ~ '/[' ~ $<MiniPerl6::Grammar.digits> ~ ']' }
    |
        <MiniPerl6::Grammar.sigil> 
        <MiniPerl6::Grammar.twigil> 
        <MiniPerl6::Grammar.full_ident> 
        {
            return ::Rul::Var( 
                    sigil  => ~$<MiniPerl6::Grammar.sigil>,
                    twigil => ~$<MiniPerl6::Grammar.twigil>,
                    name   => ~$<MiniPerl6::Grammar.full_ident>
                   )
        }
};

token rule_terms {
    |   <'('>
        <rule> \)
        { return ::Rul::Capture( rule => $$<rule> ) }
    |   <'<('>
        <rule>  <')>'>
        { return ::Rul::CaptureResult( rule => $$<rule> ) }
    |   <'<after'>
        <?ws> <rule> \> 
        { return ::Rul::After( rule => $$<rule> ) }
    |   <'<before'>
        <?ws> <rule> \> 
        { return ::Rul::Before( rule => $$<rule> ) }
    |   <'<!before'>
        <?ws> <rule> \> 
        # TODO
        { return { not_before => { rule => $$<rule> } } }
    |   <'<!'>
        # TODO
        <metasyntax> \> 
        { return { negate  => { metasyntax => $$<metasyntax> } } }
    |   <'<+'>
        # TODO
        <char_class>  \> 
        { return ::Rul::CharClass( chars => ~$<char_class> ) }
    |   <'<-'>
        # TODO
        <char_class> \>
        { return ::Rul::NegateCharClass( chars => ~$<char_class> ) }
    |   \< \'
        <literal> \' \>
        { return ::Rul::Constant( constant => $$<literal> ) }
    |   \< 
        [  
            <variables>   \>
            # { say "matching < variables ..." }
            {
                # say "found < hash-variable >";
                return ::Rul::InterpolateVar( var => $$<variables> )
            }
        |
            \?
            # TODO - non-capture
            <metasyntax>  \>
            { return ::Rul::Subrule( metasyntax => $$<metasyntax> ) }
        |
            # TODO
            <metasyntax>  \>
            { return ::Rul::Subrule( metasyntax => $$<metasyntax> ) }
        ]
    |   \{ 
        <parsed_code>  \}
        { return ::Rul::Block( closure => $$<parsed_code> ) }
    |   <'\\'>  
        [
# TODO
#        | [ x | X ] <[ 0..9 a..f A..F ]]>+
#          #  \x0021    \X0021
#          { return ::Rul::SpecialChar( char => '\\' ~ $/ ) }
#        | [ o | O ] <[ 0..7 ]>+
#          #  \x0021    \X0021
#          { return ::Rul::SpecialChar( char => '\\' ~ $/ ) }
#        | ( x | X | o | O ) \[ (<-[ \] ]>*) \]
#          #  \x[0021]  \X[0021]
#          { return ::Rul::SpecialChar( char => '\\' ~ $0 ~ $1 ) }
        | .
          #  \e  \E
          { return ::Rul::SpecialChar( char => $$/ ) }
        ]
    |   \. 
        { return ::Rul::Dot( dot => 1 ) }
    |   <'['> 
        <rule> <']'> 
        { return $$<rule> }

};

=for later
    |   <':::'> { return { colon => ':::' ,} }
    |   <':?'> { return { colon => ':?' ,} }
    |   <':+'> { return { colon => ':+' ,} }
    |   <'::'> { return { colon => '::' ,} }
    |   <':'> { return { colon => ':'  ,} }
    |   <'$$'> { return { colon => '$$' ,} }
    |   <'$'> { return { colon => '$'  ,} }


# TODO - parser error ???
#    |   <'^^'> { return { colon => '^^' ,} }
#    |   <'^'> { return { colon => '^'  ,} } }
#    |   <'»'> { return { colon => '>>' ,} } }
#    |   <'«'> { return { colon => '<<' ,} } }

    |   <'<<'> { return { colon => '<<' ,} }     
    |   <'>>'> { return { colon => '>>' ,} }     
    |   <':i'> 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', rule => $$<rule>, } }     
    |   <':ignorecase'> 
        <?ws> <rule> 
        { return { modifier => 'ignorecase', rule => $$<rule>, } }     
    |   <':s'> 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   rule => $$<rule>, } }     
    |   <':sigspace'> 
        <?ws> <rule> 
        { return { modifier => 'sigspace',   rule => $$<rule>, } }     
    |   <':P5'> 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  rule => $$<rule>, } }     
    |   <':Perl5'> 
        <?ws> <rule> 
        { return { modifier => 'Perl5',  rule => $$<rule>, } }     
    |   <':bytes'> 
        <?ws> <rule> 
        { return { modifier => 'bytes',  rule => $$<rule>, } }     
    |   <':codes'> 
        <?ws> <rule> 
        { return { modifier => 'codes',  rule => $$<rule>, } }     
    |   <':graphs'> 
        <?ws> <rule> 
        { return { modifier => 'graphs', rule => $$<rule>, } }     
    |   <':langs'> 
        <?ws> <rule> 
        { return { modifier => 'langs',  rule => $$<rule>, } } }
};
=cut

token term {
    |  
       # { say "matching variables" } 
       <variables>
       [  <?ws>? <':='> <?ws>? <named_capture_body>
          { 
            return ::Rul::NamedCapture(
                rule =>  $$<named_capture_body>,
                ident => $$<variables>
            ); 
          }
       |
          { 
            return $$<variables>
          }
       ]
    | 
        # { say "matching terms"; }
        <rule_terms>
        { 
            #print "term: ", Dumper( $_[0]->data );
            return $$<rule_terms> 
        }
    |  <-[ \] \} \) \> \: \? \+ \* \| \& \/ ]>    # TODO - <...>* - optimize!
        { return ::Rul::Constant( constant => $$/ ) }
};

token quant {
    |   <'**'> <?MiniPerl6::Grammar.opt_ws> \{  <parsed_code>  \}
        { return { closure => $$<parsed_code> } }
    |   [  \? | \* | \+  ]
};

token greedy {   \?  |  \+  |  <''>  };

token quantifier {
    |   <?MiniPerl6::Grammar.opt_ws>
        <before   \}  |  \]   |  \)   >
        XXX   # fail
    |
        <?MiniPerl6::Grammar.opt_ws>
        <term> 
        <?MiniPerl6::Grammar.opt_ws2>
        [
            <quant> <greedy>
            <?MiniPerl6::Grammar.opt_ws3>
            { return ::Rul::Quantifier(
                    term    => $$/{'term'},
                    quant   => $$/{'quant'},
                    greedy  => $$/{'greedy'},
                    ws1     => $$/{'MiniPerl6::Grammar.opt_ws'},
                    ws2     => $$/{'MiniPerl6::Grammar.opt_ws2'},
                    ws3     => $$/{'MiniPerl6::Grammar.opt_ws3'},
                )
            }
        |
            { return $$<term> }
        ]
};

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
};

token concat {
    <concat_list>
    { return ::Rul::Concat( 'concat' => $$<concat_list> ) }
};

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
};

token rule {
    [ <?ws>? <'|'> | <''> ]
    # { say "trying M::G::Rule on ", $s }
    <or_list>
    { 
        # say "found Rule";
        return ::Rul::Or( 'or' => $$<or_list> ) 
    }
};

=pod 
# -- this depends on changing the specs
#token concat {
#    <quantifier>+
#    { return Rul::Concat( concat => $<quantifier> ) }
#}
=cut

}

