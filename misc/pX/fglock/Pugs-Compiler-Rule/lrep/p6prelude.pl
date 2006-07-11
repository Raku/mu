# This is the Perl 6 Grammar used to Parse and generate the 
# Abstract Syntax Tree (AST) for Rules in PCR - fglock
#
# This code is compiled and executed by using the main lrep compiler.
#
# Things that go in this file are:
# - anything that does *not* contain a statement like:
#       eval( '...', :lang<perl5>);
# - anything that directly alters the Perl 6 Grammar
# - everything else go into the p6primitives.pl file, or into
#   a Module

# this is a comment - putter++ for writing the comment regex

grammar grammar1;

# rule xxx :P5 {foo}
# XXX - rewrite this!
rule perl5_regex { 
    [   
        \.   |   \|   |   \*   |   \+   |
        \(   |   \)   |   \[   |   \]   |
        \?   |   \:   |   \s   |   \w   | 
        \_   |   \\   |   \^   |   \$   |
        \n   |   \#   |   \-   |   \<   |
        \>   |   \!   |   \%   |   \@   | 
        alnum  |  digit
    ]*
        { return { perl5_regex => $() ,} }
}

rule perl5_rule_decl {
    rule <p6ws> <ident> <p6ws>? \: P5 <p6ws> \{ <perl5_regex> \}
        { return { perl5_rule_decl => $() ,} }
}
push @grammar1::statements, \&perl5_rule_decl;
 
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

rule rule {
    [ <?alt> | <?quantifier> ]*
}

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
    \$ \< <ident> \> <?p6ws>? \:\= <?p6ws>? \( <rule> \) 
        { return { named_capture => $() ,} }
}
unshift @rule_terms, \&named_capture;

rule immediate_statement_rule {
    <?p6ws>? <@grammar1::statements> <?p6ws>?
}

rule grammar {
    <immediate_statement_exec>*
}

rule rule_decl {
    rule <p6ws> <ident> <p6ws>? \{ <rule> \}
        { return { rule_decl => $() ,} }
}
push @grammar1::statements, \&rule_decl;
        
rule grammar_name {
    grammar <p6ws> <ident> <p6ws>? \;
        { return { grammar_name => $() ,} }
}
push @statements, \&grammar_name;

rule _push {
    $op := (push|unshift) <p6ws> <variable> <p6ws>? \, <p6ws>?
    $code := (.*?) <p6ws>? \;
        { return { _push => $() ,} }
}
push @statements, \&_push;


rule pod { 
    \=[pod|head1|kwid|for] 
    .*? 
    \=cut 
}
push @statements, \&pod;

rule use_v6 { 
    use <?p6ws> v6 \-pugs <?p6ws>? \;
}
push @statements, \&use_v6;
        
rule term1 {
    <@grammar1::terms>
}
        
rule list {
    [ <term1> <?p6ws>? \, <?p6ws>? ]* <term1>?
}

rule block {
    \{ 
        $list := ( [ <?p6ws>? <@grammar1::statements> ]* ) <?p6ws>? 
    \}
        { return { block => $()<list> ,} }
}
push @statements, \&block;

rule macro_decl {
    macro <?p6ws> $prefix := (<word>) \: \< $id := (.*?) \> <?p6ws>? 
    \(  <?p6ws>? <list> <?p6ws>? \) <?p6ws>?
    is <?p6ws> parsed <?p6ws>? \( 
        <?p6ws>? \/ <?p6ws>? <rule> <?p6ws>? \/ <?p6ws>? 
        \) <?p6ws>?
    <code> 
        { return { macro => $() ,} }
}
push @statements, \&macro_decl;

push @terms, \&variable;
push @terms, \&literal;
        
rule _print { 
    $op := (print|say|warn|die) <p6ws> <list> <p6ws>? \;
        { return { _print => $() ,} }
}
push @statements, \&_print;

rule _my {
    $op := (my|our|local) <p6ws> <variable> <p6ws>? \;
        { return { _my => $() ,} }
}
push @statements, \&_my;

rule _simple_statement {
    $op := (die|\.\.\.) \;
        { return { _simple_statement => $() ,} }
}
push @statements, \&_simple_statement;

rule sub_decl {
    sub <?p6ws> $fix := (infix|prefix|postfix) 
        \: \< $id := (.*?) \> <?p6ws>? <block>
        { return { sub_decl => $() ,} }
}
push @statements, \&sub_decl;

rule term2 {
    $term1 := (<term1>) <p6ws>? 
    $op    := (<@grammar1::ops>) <p6ws>? 
    $term2 := (<term1>) 
        { return { sub_application_term => $() ,} }
}

rule sub_application {
    $term1 := (<term1>|<term2>) <p6ws>? 
    $op    := (<@grammar1::ops>) <p6ws>? 
    $term2 := (<term1>|<term2>) <p6ws>? \;
        { return { sub_application => $() ,} }
}
push @statements, \&sub_application;

rule eval_perl5 {
    eval <p6ws>? \( <p6ws>? 
        <literal> <p6ws>? \, <p6ws>? 
        \: lang \< perl5 \> <p6ws>? 
    \) <p6ws>? \;
        { return { eval_perl5 => $<literal> } }
}
push @statements, \&eval_perl5;

rule _return {
    return <?p6ws> $val := (<term1>|<term2>) <?p6ws>? \;
        { return { _return => $() ,} }
}
push @statements, \&_return;

