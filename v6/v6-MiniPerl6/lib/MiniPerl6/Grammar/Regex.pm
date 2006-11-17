
use v6-alpha;

grammar MiniPerl6::Grammar::Regex {

my %rule_terms;

token ws {  <?MiniPerl6::Grammar.ws>  };

token ident {  <?MiniPerl6::Grammar.full_ident> | <digit> };

token any { . };

token literal {
    |  \\ .        <literal>
    |  <!before \' > .  <literal>
    |  <''>
};

token metasyntax {
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code> \}
    |  \<  <?metasyntax>  \>
    |  <!before \> > . 
    ]
    [ <metasyntax> | <''> ]
};

token char_range {
    [ 
    |  \\ .
    |  <!before \] > . 
    ]
    [ <char_range> | <''> ]
};

token char_class {
    |  <?ident>
    |  \[  <?char_range>  \]
};

# XXX - not needed
token string_code {
    # bootstrap 'code'
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?string_code> \}
    |  <!before \} > . 
    ]
    [ <string_code> | <''> ]
};

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    # XXX - call MiniPerl6 'Statement List'
    <?string_code>
    { return ~$/ }
};

token named_capture_body {
    | \(  <rule>        \)  { return { 'capturing_group' => $$<rule> ,} } 
    | \[  <rule>        \]  { return $$<rule> } 
    | \<  <metasyntax>  \>  
            { return ::Rul::Subrule( 'metasyntax' => $$<metasyntax> ) }
    | { die 'invalid alias syntax' }
};

token variables {
    |
        <'$<'>
        <ident> \> 
        { return '$/{' ~ '\'' ~ $<ident> ~ '\'' ~ '}' }
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
                    'sigil'  => ~$<MiniPerl6::Grammar.sigil>,
                    'twigil' => ~$<MiniPerl6::Grammar.twigil>,
                    'name'   => ~$<MiniPerl6::Grammar.full_ident>
                   )
        }
};

token rule_terms {
    |   <'('>
        <rule> \)
        { return ::Rul::Capture( 'rule' => $$<rule> ) }
    |   <'<('>
        <rule>  <')>'>
        { return ::Rul::CaptureResult( 'rule' => $$<rule> ) }
    |   <'<after'>
        <?ws> <rule> \> 
        { return ::Rul::After( 'rule' => $$<rule> ) }
    |   <'<before'>
        <?ws> <rule> \> 
        { return ::Rul::Before( 'rule' => $$<rule> ) }
    |   <'<!before'>
        <?ws> <rule> \> 
        { return ::Rul::NotBefore( 'rule' => $$<rule> ) }
    |   <'<!'>
        # TODO
        <metasyntax> \> 
        { return { negate  => { 'metasyntax' => $$<metasyntax> } } }
    |   <'<+'>
        # TODO
        <char_class>  \> 
        { return ::Rul::CharClass( 'chars' => ~$<char_class> ) }
    |   <'<-'>
        # TODO
        <char_class> \>
        { return ::Rul::NegateCharClass( 'chars' => ~$<char_class> ) }
    |   \< \'
        <literal> \' \>
        { return ::Rul::Constant( 'constant' => $$<literal> ) }
    |   \< 
        [  
            <variables>   \>
            # { say 'matching < variables ...' }
            {
                # say 'found < hash-variable >';
                return ::Rul::InterpolateVar( 'var' => $$<variables> )
            }
        |
            \?
            # TODO 
            <metasyntax>  \>
            { return ::Rul::SubruleNoCapture( 'metasyntax' => $$<metasyntax> ) }
        |
            # TODO
            <metasyntax>  \>
            { return ::Rul::Subrule( 'metasyntax' => $$<metasyntax> ) }
        ]
    |   \{ 
        <parsed_code>  \}
        { return ::Rul::Block( 'closure' => $$<parsed_code> ) }
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
        | <any>
          #  \e  \E
          { return ::Rul::SpecialChar( 'char' => $$<any> ) }
        ]
    |   \. 
        { return ::Rul::Dot( 'dot' => 1 ) }
    |   <'['> 
        <rule> <']'> 
        { return $$<rule> }

};

=for later
    |   <':::'> { return { 'colon' => ':::' ,} }
    |   <':?'> { return { 'colon' => ':?' ,} }
    |   <':+'> { return { 'colon' => ':+' ,} }
    |   <'::'> { return { 'colon' => '::' ,} }
    |   <':'> { return { 'colon' => ':'  ,} }
    |   <'$$'> { return { 'colon' => '$$' ,} }
    |   <'$'> { return { 'colon' => '$'  ,} }


# TODO - parser error ???
#    |   <'^^'> { return { 'colon' => '^^' ,} }
#    |   <'^'> { return { 'colon' => '^'  ,} } }
#    |   <'»'> { return { 'colon' => '>>' ,} } }
#    |   <'«'> { return { 'colon' => '<<' ,} } }

    |   <'<<'> { return { 'colon' => '<<' ,} }     
    |   <'>>'> { return { 'colon' => '>>' ,} }     
    |   <':i'> 
        <?ws> <rule> 
        { return { 'modifier' => 'ignorecase', 'rule' => $$<rule>, } }     
    |   <':ignorecase'> 
        <?ws> <rule> 
        { return { 'modifier' => 'ignorecase', 'rule' => $$<rule>, } }     
    |   <':s'> 
        <?ws> <rule> 
        { return { 'modifier' => 'sigspace',   'rule' => $$<rule>, } }     
    |   <':sigspace'> 
        <?ws> <rule> 
        { return { 'modifier' => 'sigspace',   'rule' => $$<rule>, } }     
    |   <':P5'> 
        <?ws> <rule> 
        { return { 'modifier' => 'Perl5',  'rule' => $$<rule>, } }     
    |   <':Perl5'> 
        <?ws> <rule> 
        { return { 'modifier' => 'Perl5',  'rule' => $$<rule>, } }     
    |   <':bytes'> 
        <?ws> <rule> 
        { return { 'modifier' => 'bytes',  'rule' => $$<rule>, } }     
    |   <':codes'> 
        <?ws> <rule> 
        { return { 'modifier' => 'codes',  'rule' => $$<rule>, } }     
    |   <':graphs'> 
        <?ws> <rule> 
        { return { 'modifier' => 'graphs', 'rule' => $$<rule>, } }     
    |   <':langs'> 
        <?ws> <rule> 
        { return { 'modifier' => 'langs',  'rule' => $$<rule>, } } }
};
=cut

token term {
    |  
       # { say 'matching variables' } 
       <variables>
       [  <?ws>? <':='> <?ws>? <named_capture_body>
          { 
            return ::Rul::NamedCapture(
                'rule' =>  $$<named_capture_body>,
                'ident' => $$<variables>
            ); 
          }
       |
          { 
            return $$<variables>
          }
       ]
    | 
        # { say 'matching terms'; }
        <rule_terms>
        { 
            #print 'term: ', Dumper( $_[0]->data );
            return $$<rule_terms> 
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { return ::Rul::Constant( 'constant' => $$<any> ) }
};

token quant {
    |   <'**'> <?MiniPerl6::Grammar.opt_ws> \{  <parsed_code>  \}
        { return { 'closure' => $$<parsed_code> } }
    |   [  \? | \* | \+  ]
};

token greedy {   \?  |  \+  |  <''>  };

token quantifier {
    #|   <?MiniPerl6::Grammar.opt_ws>
    #    <before   \}  |  \]   |  \)   >
    #    XXX   # fail
    #|
        <?MiniPerl6::Grammar.opt_ws>
        <term> 
        <?MiniPerl6::Grammar.opt_ws2>
        [
            <quant> <greedy>
            <?MiniPerl6::Grammar.opt_ws3>
            { return ::Rul::Quantifier(
                    'term'    => $$<term>,
                    'quant'   => $$<quant>,
                    'greedy'  => $$<greedy>,
                    'ws1'     => $$<MiniPerl6::Grammar.opt_ws>,
                    'ws2'     => $$<MiniPerl6::Grammar.opt_ws2>,
                    'ws3'     => $$<MiniPerl6::Grammar.opt_ws3>,
                )
            }
        |
            { return $$<term> }
        ]
};

token concat_list {
    <quantifier>
    [
        <concat_list> 
        { return [ $$<quantifier>, @($$<concat_list>) ] }
    |
        { return [ $$<quantifier> ] }
    ]
    |
        { return [] }
};

token concat {
    <concat_list>
    { return ::Rul::Concat( 'concat' => $$<concat_list> ) }
};

token or_list {
    <concat>
    [
        <'|'>
        <or_list> 
        { return [ $$<concat>, @($$<or_list>) ] }
    |
        { return [ $$<concat> ] }
    ]
    |
        { return [] }
};

token rule {
    [ <?ws>? <'|'> | <''> ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list>
    { 
        # say 'found Rule';
        return ::Rul::Or( 'or' => $$<or_list> ) 
    }
};

}

=begin

=head1 NAME 

MiniPerl6::Grammar::Regex - Grammar for MiniPerl6 Regex

=head1 SYNOPSIS

    my $match := $source.rule;
    ($$match).perl;    # generated Regex AST

=head1 DESCRIPTION

This module generates a syntax tree for the Regex compiler.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
