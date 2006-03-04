grammar grammar1;
        
rule pod { 
    \=[pod|head1|kwid|for] 
    .*? 
    \=cut 
}
push @statements, \&pod;
        
=kwid

pX/Common/iterator_engine_p6.pl - fglock

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
        ( [ <?ws>? <@grammar1::statements> ]* ) <?ws>? 
    \}
        { return { block => $<0> ,} }
}
push @statements, \&block;

rule macro_decl {
    macro <?ws> $prefix := (<word>) \: \< $id := (.*?) \> <?ws>? 
    \(  <?ws>? <list> <?ws>? \) <?ws>?
    is <?ws> parsed <?ws>? \( <?ws>? \/ <?ws>? <rule> <?ws>? \/ <?ws>? \) <?ws>?
    <code> 
        { return { macro => $<> ,} }
}
push @statements, \&macro_decl;

push @terms, \&variable;
push @terms, \&literal;
        
rule _print { 
    $op := (print|say|warn|die) <ws> <list> <ws>? \;
        { return { _print => $<> ,} }
}
push @statements, \&_print;

rule _my {
    $op := (my|our|local) <ws> <variable> <ws>? \;
        { return { _my => $<> ,} }
}
push @statements, \&_my;

rule _simple_statement {
    $op := (die|\.\.\.) \;
        { return { _simple_statement => $<> ,} }
}
push @statements, \&_simple_statement;

rule sub_decl {
    sub <ws> $fix := (infix|prefix|postfix) \: \< $id := (.*?) \> <ws>? <block>
        { return { sub_decl => $<> ,} }
}
push @statements, \&sub_decl;

rule term2 {
    $term1 := (<term1>) <ws>? 
    $op    := (<@grammar1::ops>) <ws>? 
    $term2 := (<term1>) 
        { return { sub_application_term => $<> ,} }
}

rule sub_application {
    $term1 := (<term1>|<term2>) <ws>? 
    $op    := (<@grammar1::ops>) <ws>? 
    $term2 := (<term1>|<term2>) <ws>? \;
        { return { sub_application => $<> ,} }
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
        { return { _return => $<> ,} }
}
push @statements, \&_return;

sub infix:<*> { eval(' $_[0] * $_[1] ', :lang<perl5>); }
sub infix:<+> { eval(' $_[0] + $_[1] ', :lang<perl5>); }
sub infix:<~> { eval(' $_[0] . $_[1] ', :lang<perl5>); }

# Current weather in Nome Alaska.
# 16.2 °F  / -8.8 °C
# Snow Showers
# Blowing Snow
# Showers Mist
