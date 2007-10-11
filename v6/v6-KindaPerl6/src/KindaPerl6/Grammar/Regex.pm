
use v6-alpha;

grammar KindaPerl6::Grammar::Regex {

my %rule_terms;

token ws {  <?KindaPerl6::Grammar.ws>  };

token ident {  <?KindaPerl6::Grammar.full_ident> | <digit> };

token any { . };

token literal {
    |  \\ .        <literal>
    |  <!before \' > .  <literal>
    |  ''
};

token metasyntax {
    [ 
    |  \\ .
    |  \'  <?literal>     \'
    |  \{  <?parsed_code>     
    |  \<  <?metasyntax>  \>
    |  <!before \> > . 
    ]
    [ <metasyntax> | '' ]
};

token char_range {
    [ 
    |  \\ .
    |  <!before \] > . 
    ]
    [ <char_range> | '' ]
};

token char_class {
    |  <?ident>
    |  \[  <?char_range>  \]
};

token parsed_code {
    <?KindaPerl6::Grammar.opt_ws>
    { 
        COMPILER::add_pad( $KindaPerl6::Grammar::Class_name );
    }
    <KindaPerl6::Grammar.exp_stmts>
    <?KindaPerl6::Grammar.opt_ws>
    '}'
    {
        my $env := @COMPILER::PAD[0];
        COMPILER::drop_pad();
        return ::Lit::Code(
                    pad   => $env,
                    state => { },
                    sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                    body  => $$<KindaPerl6::Grammar.exp_stmts>,
                );
    }
};

token named_capture_body {
    | \(  <rule>        \)  { return ::Rule::Capture( 'rule' => $$<rule> ) }
    | \[  <rule>        \]  { return $$<rule> } 
    | \<  <metasyntax>  \>  
            { return ::Rule::Subrule( 'metasyntax' => $$<metasyntax> ) }
    | { die 'invalid alias syntax' }
};

token variables {
    |
        '$<'
        <ident> \> 
        { return '$/{' ~ '\'' ~ $<ident> ~ '\'' ~ '}' }
    |
        # TODO
        <KindaPerl6::Grammar.sigil> 
        <KindaPerl6::Grammar.digits>
        { return $<KindaPerl6::Grammar.sigil> ~ '/[' ~ $<KindaPerl6::Grammar.digits> ~ ']' }
    |
        <KindaPerl6::Grammar.sigil> 
        <KindaPerl6::Grammar.twigil> 
        <KindaPerl6::Grammar.full_ident> 
        {
            return ::Rule::Var( 
                    'sigil'  => ~$<KindaPerl6::Grammar.sigil>,
                    'twigil' => ~$<KindaPerl6::Grammar.twigil>,
                    'name'   => ~$<KindaPerl6::Grammar.full_ident>
                   )
        }
};

token assertion_modifier { '!' | '?' | '' };

token rule_terms {
    |   '('
        <rule> \)
        { return ::Rule::Capture( 'rule' => $$<rule> ) }
    |   '<('
        <rule>  ')>'
        { return ::Rule::CaptureResult( 'rule' => $$<rule> ) }
    |   '<' <assertion_modifier> 'after'
        <?ws> <rule> \> 
        { return ::Rule::After( 'rule' => $$<rule>, 'assertion_modifier' => $$<assertion_modifier> ) }
    |   '<' <assertion_modifier> 'before'
        <?ws> <rule> \> 
        { return ::Rule::Before( 'rule' => $$<rule>, 'assertion_modifier' => $$<assertion_modifier> ) }
    # |   '<!before'
    #    <?ws> <rule> \> 
    #    { return ::Rule::NotBefore( 'rule' => $$<rule> ) }
    |   '<!'
        # TODO
        <metasyntax> \> 
        { return { negate  => { 'metasyntax' => $$<metasyntax> } } }
    |   '<+'
        # TODO
        <char_class>  \> 
        { return ::Rule::CharClass( 'chars' => ~$<char_class> ) }
    |   '<-'
        # TODO
        <char_class> \>
        { return ::Rule::NegateCharClass( 'chars' => ~$<char_class> ) }
    |   \'
        <literal> \' 
        { return ::Rule::Constant( 'constant' => $$<literal> ) }
    |   \< 
        [  
            <variables>   \>
            # { say 'matching < variables ...' }
            {
                # say 'found < hash-variable >';
                return ::Rule::InterpolateVar( 'var' => $$<variables> )
            }
        |
            \?
            # TODO 
            <metasyntax>  \>
            { return ::Rule::SubruleNoCapture( 'metasyntax' => $$<metasyntax> ) }
        |
            # TODO
            <metasyntax>  \>
            { return ::Rule::Subrule( 'metasyntax' => $$<metasyntax> ) }
        ]
    |   \{ 
        <parsed_code>  
        { return ::Rule::Block( 'closure' => $$<parsed_code> ) }
    |   <KindaPerl6::Grammar.backslash>  
        [
# TODO
#        | [ x | X ] <[ 0..9 a..f A..F ]]>+
#          #  \x0021    \X0021
#          { return ::Rule::SpecialChar( char => '\\' ~ $/ ) }
#        | [ o | O ] <[ 0..7 ]>+
#          #  \x0021    \X0021
#          { return ::Rule::SpecialChar( char => '\\' ~ $/ ) }
#        | ( x | X | o | O ) \[ (<-[ \] ]>*) \]
#          #  \x[0021]  \X[0021]
#          { return ::Rule::SpecialChar( char => '\\' ~ $0 ~ $1 ) }
        | <any>
          #  \e  \E
          { return ::Rule::SpecialChar( 'char' => $$<any> ) }
        ]
    |   \. 
        { return ::Rule::Dot( 'dot' => 1 ) }
    |   '[' 
        <rule> ']' 
        { return $$<rule> }

};


token term {
    |  
       # XXX special case for $<xxx> := (...) 
       '$<' <ident> '>' <?ws>? ':=' <?ws>? '(' <rule> ')'
          { 
            return ::Rule::NamedCapture(
                'rule' =>  $$<rule>,
                'ident' => $$<ident>
            ); 
          }
    | 
       # { say 'matching variables' } 
       <variables>
       [  <?ws>? ':=' <?ws>? <named_capture_body>
          { 
            return ::Rule::NamedCapture(
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
        { return ::Rule::Constant( 'constant' => $$<any> ) }
};

token quant {
    |   '**' <?KindaPerl6::Grammar.opt_ws> \{  <parsed_code>  
        { return { 'closure' => $$<parsed_code> } }
    |   [  \? | \* | \+  ]
};

token greedy {   \?  |  \+  |  ''  };

token quantifier {
    #|   <?KindaPerl6::Grammar.opt_ws>
    #    <before   \}  |  \]   |  \)   >
    #    XXX   # fail
    #|
        <?KindaPerl6::Grammar.opt_ws>
        <term> 
        <?KindaPerl6::Grammar.opt_ws2>
        [
            <quant> <greedy>
            <?KindaPerl6::Grammar.opt_ws3>
            { return ::Rule::Quantifier(
                    'term'    => $$<term>,
                    'quant'   => $$<quant>,
                    'greedy'  => $$<greedy>,
                    'ws1'     => $$<KindaPerl6::Grammar.opt_ws>,
                    'ws2'     => $$<KindaPerl6::Grammar.opt_ws2>,
                    'ws3'     => $$<KindaPerl6::Grammar.opt_ws3>,
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
    { return ::Rule::Concat( 'concat' => $$<concat_list> ) }
};

token or_list {
    <concat>
    [
        '|'
        <or_list> 
        { return [ $$<concat>, @($$<or_list>) ] }
    |
        { return [ $$<concat> ] }
    ]
    |
        { return [] }
};

token rule {
    [ <?ws>? '|' | '' ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list>
    { 
        # say 'found Rule';
        return ::Rule::Or( 'or' => $$<or_list> ) 
    }
};

}

=begin

=head1 NAME 

KindaPerl6::Grammar::Regex - Grammar for KindaPerl6 Regex

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

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
