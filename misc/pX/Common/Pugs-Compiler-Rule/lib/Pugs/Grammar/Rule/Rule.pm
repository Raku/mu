use v6-pugs;

grammar Pugs::Grammar::Rule;

=head1 NAME

Pugs::Grammar::Rule - Perl 6 grammar for rules

=head1 DESCRIPTION

This is the Perl 6 Grammar used to Parse and generate the 
Abstract Syntax Tree (AST) for Rules.

=cut
 
rule word     :P5 {^([_[:alnum:]]+)}
rule any      :P5 {^(.)}
rule escaped_char  
              :P5 {^\\(.)}
rule newline  :P5 {^(\n)}
rule ws       :P5 {^(\s+)}
rule p6ws     :P5 {^((?:\s|\#(?-s:.)*)+)}

# XXX - set non-capture flag?
# XXX - incomplete - needs a return block
rule non_capturing_subrule
              :P5 {^\<\?(.*?)\>}
push @rule_terms, \&non_capturing_subrule;

# XXX - incomplete - needs a return block
rule negated_subrule
              :P5 {^\<\!(.*?)\>}
push @rule_terms, \&negated_subrule;

# XXX - incomplete - needs a return block
rule subrule  :P5 {^\<(.*?)\>}
push @rule_terms, \&subrule;

rule const_word {
    <word>
        { return { constant => $() ,} }
}
unshift @rule_terms, \&const_word;

rule const_escaped_char {
    <escaped_char> 
        { return { constant => $() ,} }
}
unshift @rule_terms, \&const_escaped_char;

rule dot {
    (\.) 
        { return { dot => $() ,} }
}
unshift @rule_terms, \&dot;

rule non_capturing_group {
     \[ <?rule> \] 
}
push @rule_terms, \&non_capturing_group;

rule closure_rule {
    <code>
        { return { closure => $() ,} }
}
unshift @rule_terms, \&closure_rule;

rule variable_rule {
    <variable> 
        { return { variable => $() ,} }
}
unshift @rule_terms, \&variable_rule;

rule runtime_alternation {
    \< <variable> \>
        { return { runtime_alternation => $() ,} }
}
unshift @rule_terms, \&runtime_alternation;

rule named_capture {
    \$ <ident> <?p6ws>? \:\= <?p6ws>? \( <rule> \) 
        { return { named_capture => $() ,} }
}
unshift @rule_terms, \&named_capture;


rule capturing_group {
    \( <rule> \)
        { return { capturing_group => $() } }
}
unshift @rule_terms, \&capturing_group;


rule constant {
    \< <literal> \>
        { return { constant => $() } }
}
unshift @rule_terms, \&constant;


sub term {
    <?p6ws>? <@rule_terms> <?p6ws>
}


sub quantifier {
    ...
    |
    <term>
}

=for translate
    Pugs::Runtime::Rule::alternation( [
        Pugs::Runtime::Rule::capture( 'star', 
            Pugs::Runtime::Rule::concat(
                Pugs::Runtime::Rule::capture( 'term', \&term ),
                Pugs::Runtime::Rule::capture( 'literal',
                    Pugs::Runtime::Rule::alternation( [
                        Pugs::Runtime::Rule::constant( '??' ),
                        Pugs::Runtime::Rule::constant( '?' ),
                        Pugs::Runtime::Rule::constant( '*?' ),
                        Pugs::Runtime::Rule::constant( '+?' ),
                        Pugs::Runtime::Rule::constant( '*' ),
                        Pugs::Runtime::Rule::constant( '+' ),
                    ] ),
                ),
                \&p6ws_star,
            ),
        ),
        \&term,
    ] );
=cut

rule rule {
    [ <?alt> | <?quantifier> ]*
}

