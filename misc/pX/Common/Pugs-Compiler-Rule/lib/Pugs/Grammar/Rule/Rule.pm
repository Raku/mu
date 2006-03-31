use v6-pugs;

grammar Pugs::Grammar::Rule;

=head1 NAME

Pugs::Grammar::Rule - Perl 6 grammar for rules

=head1 DESCRIPTION

This is the Perl 6 Grammar used to Parse and generate the 
Abstract Syntax Tree (AST) for Rules.

=cut
 
rule p6ws     :P5 {^((?:\s|\#(?-s:.)*)+)}

rule variable :P5 {^([\$\%\@](?:(?:\:\:)?[_[:alnum:]]+)+)}

rule ident    :P5 {^((?:(?:\:\:)?[_[:alnum:]]+)+)}

rule num_variable :P5 {^(?:\$[[:digit:]]+)}

rule escaped_char :P5 {^\\(.)}

# terms

    rule dot {
        \.    
            
        { return { 'dot' => 1 ,} }
    }
    unshift @rule_terms, 'dot';
    
    # \w not implemented in lrep...
    rule _word_char    :P5 {^([[:alnum:]])}
    rule word {
        <_word_char>    
            
        { return { 'constant' => $_[0]{_word_char}() ,} }
    }
    unshift @rule_terms, 'word';
    
    rule special_char {
        <escaped_char>

        { return { special_char => $_[0]{escaped_char}(), } } 
    }
    unshift @rule_terms, 'special_char';
    
    rule non_capturing_group {
        \[ <rule> \] 
         
        { return $_[0]{rule}() }
    }
    push @rule_terms, 'non_capturing_group';
    
    rule closure_rule {
        <code> 
            
        { return { closure => $_[0]{code}() ,} }
    }
    unshift @rule_terms, 'closure_rule';
    
    rule variable_rule {
        <variable> 
            
        { return { variable => $() ,} }
    }
    unshift @rule_terms, 'variable_rule';
    
    # $0 $1
    rule match_variable {
        <num_variable>    
            
        { return { match_variable => $_[0]{num_variable}() ,} }
    }
    unshift @rule_terms, 'match_variable';
    
    rule named_capture {
        \$ \< <ident> \> <?p6ws>? \:\= <?p6ws>? \( <rule> \) 
        
        { return { named_capture => {
                ident => $_[0]{ident}(),
                rule  => $_[0]{rule}(),
            }, } 
        }
    }
    unshift @rule_terms, 'named_capture';
        
    rule before {
        \< before <?p6ws> <rule> \> 
        
        { return { before => {
                rule  => $_[0]{rule}(),
            }, } 
        }
    }
    unshift @rule_terms, 'before';
        
    rule capturing_group {
        \( <rule> \)
            
        { return { capturing_group => $_[0]{rule}() ,} }
    }
    unshift @rule_terms, 'capturing_group';
    
    rule colon1 {
        \:
            
        { return { colon => 1 ,} }
    }
    push @rule_terms, 'colon1';
    
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

rule concat {
    $<q1> := (<quantifier>) 
    [
        $<q2> := (<concat>) 
        
        { return { concat => [ 
                { quant => $_[0]{q1}() ,}, 
                $_[0]{q2}(),
            ] ,} 
        } 
    
    ]?
    
    { return { quant => $_[0]{q1}() ,} } 
}

rule rule {
    $<q1> := (<concat>) 
    [
        \| $<q2> := (<rule>) 

        { return { alt => [ 
                $_[0]{q1}(), 
                $_[0]{q2}(),
            ] ,} 
        }
    
    ]?
            
    { return $_[0]{q1}() } 
}
