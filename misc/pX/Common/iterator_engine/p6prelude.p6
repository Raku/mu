grammar grammar1;

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
    \$ <ident> <?ws>? \:\= <?ws>? \( <rule> \) 
        { return { named_capture => $() ,} }
}
unshift @rule_terms, \&named_capture;

rule immediate_statement_rule {
    <?ws>? <@grammar1::statements> <?ws>?
}

rule grammar {
    <immediate_statement_exec>*
}

rule rule_decl {
    rule <ws> <ident> <ws>? \{ <rule> \}
        { return { rule_decl => $() ,} }
}
push @grammar1::statements, \&rule_decl;
        
rule grammar_name {
    grammar <ws> <ident> <ws>? \;
        { return { grammar_name => $() ,} }
}
push @statements, \&grammar_name;

rule _push {
    $op := (push|unshift) <ws> <variable> <ws>? \, <ws>?
    $code := (.*?) <ws>? \;
        { return { _push => $() ,} }
}
push @statements, \&_push;


rule pod { 
    \=[pod|head1|kwid|for] 
    .*? 
    \=cut 
}
push @statements, \&pod;
        
=kwid

pX/Common/p6.pl - fglock

- experimental implementation of a grammar that could parse Perl 6 

=cut
    
rule term1 {
    <@grammar1::terms>
}
        
rule list {
    [ <term1> <?ws>? \, <?ws>? ]* <term1>?
}

rule block {
    \{ 
        $list := ( [ <?ws>? <@grammar1::statements> ]* ) <?ws>? 
    \}
        { return { block => $()<list> ,} }
}
push @statements, \&block;

rule macro_decl {
    macro <?ws> $prefix := (<word>) \: \< $id := (.*?) \> <?ws>? 
    \(  <?ws>? <list> <?ws>? \) <?ws>?
    is <?ws> parsed <?ws>? \( <?ws>? \/ <?ws>? <rule> <?ws>? \/ <?ws>? \) <?ws>?
    <code> 
        { return { macro => $() ,} }
}
push @statements, \&macro_decl;

push @terms, \&variable;
push @terms, \&literal;
        
rule _print { 
    $op := (print|say|warn|die) <ws> <list> <ws>? \;
        { return { _print => $() ,} }
}
push @statements, \&_print;

rule _my {
    $op := (my|our|local) <ws> <variable> <ws>? \;
        { return { _my => $() ,} }
}
push @statements, \&_my;

rule _simple_statement {
    $op := (die|\.\.\.) \;
        { return { _simple_statement => $() ,} }
}
push @statements, \&_simple_statement;

rule sub_decl {
    sub <?ws> $fix := (infix|prefix|postfix) \: \< $id := (.*?) \> <?ws>? <block>
        { return { sub_decl => $() ,} }
}
push @statements, \&sub_decl;

rule term2 {
    $term1 := (<term1>) <ws>? 
    $op    := (<@grammar1::ops>) <ws>? 
    $term2 := (<term1>) 
        { return { sub_application_term => $() ,} }
}

rule sub_application {
    $term1 := (<term1>|<term2>) <ws>? 
    $op    := (<@grammar1::ops>) <ws>? 
    $term2 := (<term1>|<term2>) <ws>? \;
        { return { sub_application => $() ,} }
}
push @statements, \&sub_application;

rule eval_perl5 {
    eval <ws>? \( <ws>? 
        <literal> <ws>? \, <ws>? 
        \: lang \< perl5 \> <ws>? 
    \) <ws>? \;
        { return { eval_perl5 => $<literal> } }
}
push @statements, \&eval_perl5;


=for TODO
    reimplement print(), warn ... using 'sub'
    implement eval_perl6 and eval_block 
    
    operand fixity (infix, prefix...)
    operand precedence (+, *, ln)
    
    class
    
    find out how to change syntax while in the parse-and-generate-ast phase
    (for example, when a new sub is created)
    
    macros
=cut

rule _return {
    return <?ws> $val := (<term1>|<term2>) <?ws>? \;
        { return { _return => $() ,} }
}
push @statements, \&_return;

