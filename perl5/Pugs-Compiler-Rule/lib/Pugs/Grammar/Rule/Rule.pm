# the v6 line is commented out due to bootstraping problem.
#use v6-alpha;

grammar Pugs::Grammar::Rule;

=head1 NAME

Pugs::Grammar::Rule - Perl 6 grammar for rules

=head1 DESCRIPTION

This is the Perl 6 Grammar used to Parse and generate the 
Abstract Syntax Tree (AST) for Rules.

=cut
 
rule ws     :P5 {^((?:\s|\#(?-s:.)*)+)}

rule variable :P5 {^([\$\%\@](?:(?:\:\:)?[_[:alnum:]]+)+)}

rule positional_variable 
              :P5 {^([\$\%\@]\^(?:[_[:alnum:]]+))}

rule ident    :P5 {^((?:(?:\:\:)?[_[:alnum:]]+)+)}

rule num_variable :P5 {^(?:\$[[:digit:]]+)}


# terms

    rule dot {
        \.    
            
        { return { 'dot' => 1 ,} }
    }
    unshift @rule_terms, 'dot';
    
    rule plain_text {
        <alnum> | \, | \; | \_ | \/ | \~ | \" | \' | \=

        { return { 'constant' => $() ,} }
    }
    unshift @rule_terms, 'plain_text';
    
    rule special_char {
        \\ .

        { return { special_char => $(), } } 
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
        <variable> | <positional_variable>
            
        { return { variable => $() ,} }
    }
    unshift @rule_terms, 'variable_rule';
    
    # $0 $1
    rule match_variable {
        <num_variable>    
            
        { return { match_variable => $_[0]{num_variable}() ,} }
    }
    unshift @rule_terms, 'match_variable';

    rule named_capture_body {
          [ <capturing_group>     { return { rule => $_[0]{capturing_group}(), } } ]
        | [ <non_capturing_group> { return { rule => $_[0]{non_capturing_group}(), } } ]
        | [ <metasyntax>          { return { rule => $_[0]{metasyntax}(), } } ]
        | { die "invalid alias syntax" }
    }
    
    rule named_capture {
        \$ \< <ident> \> <?ws>? \:\= <?ws>? <named_capture_body>
        
        { my $body = $_[0]{named_capture_body}();
          $body->{ident} = $_[0]{ident}();
          return { named_capture => $body, } 
        }
    }
    unshift @rule_terms, 'named_capture';
        
    rule before {
        \< before <?ws> <rule> \> 
        
        { return { before => {
                rule  => $_[0]{rule}(),
            }, } 
        }
    }
    unshift @rule_terms, 'before';
        
    rule after {
        \< after <?ws> <rule> \> 
        
        { return { after => {
                rule  => $_[0]{rule}(),
            }, } 
        }
    }
    unshift @rule_terms, 'after';
        
    rule negate {
        \< \! 
        [
        before <?ws> <rule> \> 
        { return { 
            negate => {
              before => {
                rule  => $_[0]{rule}(),
            }, }, } 
        }
        |
        after <?ws> <rule> \> 
        { return { 
            negate => {
              after => {
                rule  => $_[0]{rule}(),
            }, }, } 
        }
        |
        <rule> \> 
        { return { negate => {
                rule  => $_[0]{rule}(),
            }, } 
        }
        ]
    }
    unshift @rule_terms, 'negate';

    rule capturing_group {
        \( <rule> \)
            
        { return { capturing_group => $_[0]{rule}() ,} }
    }
    unshift @rule_terms, 'capturing_group';
    
    rule colon {
        ( 
            [ \:\:\: ] | 
            [ \:\? ]   | 
            [ \:\+ ]   | 
            [ \:\: ]   | \: |
            [ \$\$ ]   | \$ |
            [ \^\^ ]   | \^
        )
            
        { return { colon => $_[0]->() ,} }
    }
    push @rule_terms, 'colon';
    
# /terms


rule quantifier {
    $<ws1>   := (<?ws>?)
    $<term>  := (<@Pugs::Grammar::Rule::rule_terms>)
    $<ws2>   := (<?ws>?)
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
    $<ws3>   := (<?ws>?)
    
    { return {  
            term  => $_[0]{term}(),
            quant => $_[0]{quant}(),
            ws1   => $_[0]{ws1}(),
            ws2   => $_[0]{ws2}(),
            ws3   => $_[0]{ws3}(),
        } 
    }
}

rule concat {
    $<q1> := (<quantifier>) 
    [
        $<q2> := (<concat>) 
        
        { 
            my $q2 = $_[0]{q2}();
            return { concat => [ 
                { quant => $_[0]{q1}() ,}, 
                ( exists $q2->{concat} 
                    ? @{$q2->{concat}}
                    : $q2 
                ),
            ] ,} 
        } 
    
    ]?
    
    { return { quant => $_[0]{q1}() ,} } 
}

rule rule {
    [<?ws>\|]?
    $<q1> := (<concat>) 
    [
        \| $<q2> := (<rule>) 

        { 
            my $q2 = $_[0]{q2}();
            return { alt => [ 
                $_[0]{q1}(), 
                ( exists $q2->{alt} 
                    ? @{$q2->{alt}}
                    : $q2 
                ),
            ] ,} 
        }
    
    ]?
            
    { return $_[0]{q1}() } 
}
