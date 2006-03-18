use v6-pugs;

grammar Pugs::Grammar::Rule;

=head1 NAME

Pugs::Grammar::Rule - Perl 6 grammar for rules

=head1 DESCRIPTION

This is the Perl 6 Grammar used to Parse and generate the 
Abstract Syntax Tree (AST) for Rules.

=cut
 
rule p6ws     :P5 {^((?:\s|\#(?-s:.)*)+)}

rule word     :P5 {^([_[:alnum:]]+)}
unshift @rule_terms, 'word';


rule dot      :P5 {^(.)}
unshift @rule_terms, 'dot';

rule escaped_char  
              :P5 {^\\(.)}
unshift @rule_terms, 'escaped_char';

rule non_capturing_subrule
              :P5 {^\<\?(.*?)\>}
push @rule_terms, 'non_capturing_subrule';

# XXX - incomplete - needs a return block
rule negated_subrule
              :P5 {^\<\!(.*?)\>}
push @rule_terms, 'negated_subrule';

# XXX - incomplete - needs a return block
rule subrule  :P5 {^\<(.*?)\>}
push @rule_terms, 'subrule';

rule non_capturing_group {
     \[ <?rule> \] 
        { return %{$_[0]} }
}
push @rule_terms, 'non_capturing_group';

rule closure_rule {
    <code>
        { return { closure => $() ,} }
}
unshift @rule_terms, 'closure_rule';

rule variable_rule {
    <variable> 
        { return { variable => $() ,} }
}
unshift @rule_terms, 'variable_rule';

rule runtime_alternation {
    \< <variable> \>
        { return { runtime_alternation => $() ,} }
}
unshift @rule_terms, 'runtime_alternation';

rule named_capture {
    \$ \< <ident> \> <?p6ws>? \:\= <?p6ws>? \( <rule> \) 
        { return { named_capture => $() ,} }
}
unshift @rule_terms, 'named_capture';


rule capturing_group {
    \( <rule> \)
        { return { capturing_group => \%{$_[0]} } }
}
unshift @rule_terms, 'capturing_group';


rule constant {
    \< <literal> \>
        { return { constant => $() } }
}
unshift @rule_terms, 'constant';


rule term {
    <?p6ws>? $<term1> := (<@Pugs::Grammar::Rule::rule_terms>) <?p6ws>?
        { return { %{$_[0]} ,} }
}


rule quantifier {
    <term> [ 
        [ \?\? ] |
        [ \*\? ] |
        [ \+\? ] |
        \?       |
        \*       |
        \+
      ]? 
      <?p6ws>?
        { return { quantifier => $_[0] ,} }
}

rule alt {
    <quantifier> [ \| <quantifier> ]+
        { return { alt => $_[0] ,} }
}

rule rule {
    [ <alt> | <quantifier> ]*
        { return { rule => $_[0] ,} }
}

