
use v6-alpha;

grammar KindaPerl6::Grammar::Regex {

my %rule_terms;

token ws {  <.KindaPerl6::Grammar.ws>  };

token ident {  <.KindaPerl6::Grammar.full_ident> | <digit> };

token any { . };

token literal {
    |  \\ .        <literal>
    |  <!before \' > .  <literal>
    |  ''
};

token metasyntax {
    [ 
    |  \\ .
    |  \'  <.literal>     \'
    |  \{  <.parsed_code>     
    |  \<  <.metasyntax>  \>
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
    |  <.ident>
    |  \[  <.char_range>  \]
};

token parsed_code {
    <.KindaPerl6::Grammar.opt_ws>
    { 
        COMPILER::add_pad( $KindaPerl6::Grammar::Class_name );
    }
    <KindaPerl6::Grammar.exp_stmts>
    <.KindaPerl6::Grammar.opt_ws>
    '}'
    {
        my $env := COMPILER::current_pad();
        COMPILER::drop_pad();
        make Lit::Code.new(
                    pad   => $env,
                    state => { },
                    sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                    body  => $$<KindaPerl6::Grammar.exp_stmts>,
                );
    }
};

token named_capture_body {
    | \(  <rule>        \)  { make Rule::Capture.new( 'rule' => $$<rule> ) }
    | \[  <rule>        \]  { make $$<rule> } 
    | \<  <metasyntax>  \>  
            { make Rule::Subrule.new( 'metasyntax' => $$<metasyntax> ) }
    | { die 'invalid alias syntax' }
};

token variables {
    |
        '$<'
        <ident> \> 
        { make '$/{' ~ '\'' ~ $<ident> ~ '\'' ~ '}' }
    |
        # TODO
        <KindaPerl6::Grammar.sigil> 
        <KindaPerl6::Grammar.digits>
        { make $<KindaPerl6::Grammar.sigil> ~ '/[' ~ $<KindaPerl6::Grammar.digits> ~ ']' }
    |
        <KindaPerl6::Grammar.sigil> 
        <KindaPerl6::Grammar.twigil> 
        <KindaPerl6::Grammar.full_ident> 
        {
            make Rule::Var.new( 
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
        { make Rule::Capture.new( 'rule' => $$<rule> ) }
    |   '<('
        <rule>  ')>'
        { make Rule::CaptureResult.new( 'rule' => $$<rule> ) }
    |   '<' <assertion_modifier> 'after'
        <.ws> <rule> \> 
        { make Rule::After.new( 'rule' => $$<rule>, 'assertion_modifier' => $$<assertion_modifier> ) }
    |   '<' <assertion_modifier> 'before'
        <.ws> <rule> \> 
        { make Rule::Before.new( 'rule' => $$<rule>, 'assertion_modifier' => $$<assertion_modifier> ) }
    # |   '<!before'
    #    <.ws> <rule> \> 
    #    { make Rule::NotBefore.new( 'rule' => $$<rule> ) }
    |   '<!'
        # TODO
        <metasyntax> \> 
        { make { negate  => { 'metasyntax' => $$<metasyntax> } } }
    |   '<+'
        # TODO
        <char_class>  \> 
        { make Rule::CharClass.new( 'chars' => ~$<char_class> ) }
    |   '<-'
        # TODO
        <char_class> \>
        { make Rule::NegateCharClass.new( 'chars' => ~$<char_class> ) }
    |   \'
        <literal> \' 
        { make Rule::Constant.new( 'constant' => $$<literal> ) }
    |   \< 
        [  
            <variables>   \>
            # { say 'matching < variables ...' }
            {
                # say 'found < hash-variable >';
                make Rule::InterpolateVar.new( 'var' => $$<variables> )
            }
        |
            \?
            { warn "<? ...> not implemented - maybe you mean <. ...> ?" }
            <metasyntax>  \>
            { make Rule::SubruleNoCapture.new( 'metasyntax' => $$<metasyntax> ) }
        |
            \.
            <metasyntax>  \>
            { make Rule::SubruleNoCapture.new( 'metasyntax' => $$<metasyntax> ) }
        |
            # TODO
            <metasyntax>  \>
            { make Rule::Subrule.new( 'metasyntax' => $$<metasyntax> ) }
        ]
    |   \{ 
        <parsed_code>  
        { make Rule::Block.new( 'closure' => $$<parsed_code> ) }
    |   <KindaPerl6::Grammar.backslash>  
        [
# TODO
#        | [ x | X ] <[ 0..9 a..f A..F ]]>+
#          #  \x0021    \X0021
#          { make Rule::SpecialChar.new( char => '\\' ~ $/ ) }
#        | [ o | O ] <[ 0..7 ]>+
#          #  \x0021    \X0021
#          { make Rule::SpecialChar.new( char => '\\' ~ $/ ) }
#        | ( x | X | o | O ) \[ (<-[ \] ]>*) \]
#          #  \x[0021]  \X[0021]
#          { make Rule::SpecialChar.new( char => '\\' ~ $0 ~ $1 ) }
        | <any>
          #  \e  \E
          { make Rule::SpecialChar.new( 'char' => $$<any> ) }
        ]
    |   \. 
        { make Rule::Dot.new( 'dot' => 1 ) }
    |   '[' 
        <rule> ']' 
        { make $$<rule> }

};


token term {
    |  
       # XXX special case for $<xxx> := (...) 
       '$<' <ident> '>' <.ws>? ':=' <.ws>? '(' <rule> ')'
          { 
            make Rule::NamedCapture.new(
                'rule' =>  $$<rule>,
                'ident' => $$<ident>
            ); 
          }
    | 
       # { say 'matching variables' } 
       <variables>
       [  <.ws>? ':=' <.ws>? <named_capture_body>
          { 
            make Rule::NamedCapture.new(
                'rule' =>  $$<named_capture_body>,
                'ident' => $$<variables>
            ); 
          }
       |
          { 
            make $$<variables>
          }
       ]
    | 
        # { say 'matching terms'; }
        <rule_terms>
        { 
            #print 'term: ', Dumper( $_[0]->data );
            make $$<rule_terms> 
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { make Rule::Constant.new( 'constant' => $$<any> ) }
};

token quant {
    |   '**' <.KindaPerl6::Grammar.opt_ws> \{  <parsed_code>  
        { make { 'closure' => $$<parsed_code> } }
    |   [  \? | \* | \+  ]
};

token greedy {   \?  |  \+  |  ''  };

token quantifier {
    #|   <.KindaPerl6::Grammar.opt_ws>
    #    <before   \}  |  \]   |  \)   >
    #    XXX   # fail
    #|
        <.KindaPerl6::Grammar.opt_ws>
        <term> 
        <.KindaPerl6::Grammar.opt_ws2>
        [
            <quant> <greedy>
            <.KindaPerl6::Grammar.opt_ws3>
            { make Rule::Quantifier.new(
                    'term'    => $$<term>,
                    'quant'   => $$<quant>,
                    'greedy'  => $$<greedy>,
                    'ws1'     => $$<KindaPerl6::Grammar.opt_ws>,
                    'ws2'     => $$<KindaPerl6::Grammar.opt_ws2>,
                    'ws3'     => $$<KindaPerl6::Grammar.opt_ws3>,
                )
            }
        |
            { make $$<term> }
        ]
};

token concat_list {
    <quantifier>
    [
        <concat_list> 
        { make [ $$<quantifier>, ($$<concat_list>).values ] }
    |
        { make [ $$<quantifier> ] }
    ]
    |
        { make [] }
};

token concat {
    <concat_list>
    { make Rule::Concat.new( 'concat' => $$<concat_list> ) }
};

token or_list {
    <concat>
    [
        '|'
        <or_list> 
        { make [ $$<concat>, ($$<or_list>).values ] }
    |
        { make [ $$<concat> ] }
    ]
    |
        { make [] }
};

token rule {
    [ <.ws>? '|' | '' ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list>
    { 
        # say 'found Rule';
        make Rule::Or.new( 'or' => $$<or_list> ) 
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
