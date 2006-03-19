use v6-pugs;

grammar Pugs::Grammar::Rule;

=head1 NAME

Pugs::Grammar::Rule - Perl 6 grammar for rules

=head1 DESCRIPTION

This is the Perl 6 Grammar used to Parse and generate the 
Abstract Syntax Tree (AST) for Rules.

=cut
 
rule p6ws     :P5 {^((?:\s|\#(?-s:.)*)+)}

# terms

    rule dot {
        \.    
            { return { 'dot' => 1 ,} }
    }
    unshift @rule_terms, 'dot';
    
    rule word     :P5 {^([_[:alnum:]]+)}
    unshift @rule_terms, 'word';
    
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
         \[ <rule> \] 
            { return $_[0]{rule} }
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
            { return { capturing_group => $_[0]{rule}() ,} }
    }
    unshift @rule_terms, 'capturing_group';
    
    
    rule constant {
        \< <literal> \>
            { return { constant => $() } }
    }
    unshift @rule_terms, 'constant';
    
# /terms


rule quantifier {
    <?p6ws>?
    $<term> := (<@Pugs::Grammar::Rule::rule_terms>)
    <?p6ws>?
    $<quant> := (
        [ 
            [ \?\? ] |
            [ \*\? ] |
            [ \+\? ] |
            \?       |
            \*       |
            \+
        ]?
    )
    <?p6ws>?
    
    { return {  
            term =>  $_[0]{term}(),
            quant => $_[0]{quant}(),
        } 
    }
}

rule rule {
    [ 
        $<q1> := (<quantifier>) \| $<q2> := (<quantifier>) 
        
        { return { alt => [ 
                { quant => $_[0]{q1}() }, 
                { quant => $_[0]{q2}() },
            ] ,} 
        } 
    ]
    | 
    [ 
        <quantifier> 
        
        { return { quant => $_[0]{quantifier}() ,} } 
    ]
}

