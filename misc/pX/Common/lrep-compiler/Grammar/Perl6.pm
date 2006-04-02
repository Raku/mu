package Grammar::Perl6;
use base 'Pugs::Grammar::Base', 'Pugs::Grammar::Rule', 'Grammar::Perl6Init';
use Runtime::RuleCompiler qw(rule);
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;
*{'immediate_statement_rule'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    <?p6ws>? <@Grammar::Perl6::statements> <?p6ws>?
        { return $() }
RULE
*{'grammar'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    <immediate_statement_rule>*
        { return $() }
RULE
*{'indirect_object'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

	<varscalar> <p6ws>? \:
	{ return $<varscalar> }
RULE
*{'rule_decl'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    rule <p6ws> <ident> <p6ws>? \{ <rule> \}
        { return { rule_decl => $() ,} }
RULE
*{'grammar_name'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    grammar <p6ws> <ident> <p6ws>? \;
        { return { grammar_name => $() ,} }
RULE
*{'condition_rule'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

	$<op>:=(if|unless)<?p6ws>?\(<?p6ws>?$<condition>:=(<term1>)<?p6ws>?\)
	<?p6ws>?$<then>:=(<block>)
	{ return { condition => $() } }
RULE
*{'meth_call_term'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<class>:=(<ident>) \. $<meth>:=(<word>)\(<?p6ws>?$<params>:=(<list>?)<?p6ws>?\)<?p6ws>?
        { return { meth_call_term => $() } }
RULE
*{'meth_call_statement'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<class>:=(<ident>) \. $<meth>:=(<word>)\(<?p6ws>?$<params>:=(<list>?)<?p6ws>?\)<?p6ws>?\;
        { return { meth_call => $() } }
RULE
*{'sub_call_term'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<name>:=(<ident>)\(<?p6ws>?$<params>:=(<list>?)<?p6ws>?\)<?p6ws>?
        { return { sub_call_term => $() } }
RULE
*{'sub_call_statement'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<name>:=(<ident>)<?p6ws>?\(<?p6ws>?$<params>:=(<list>?)<?p6ws>?\)<?p6ws>?\;
        { return { sub_call => $() } }
RULE
*{'access_hashref_element'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<variable>:=(<varscalar>)\{$<key>:=(<term1>)\}
        { return { access_hashref_element => $() } }
RULE
*{'access_hash_element'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<variable>:=(<varhash>)\{$<key>:=(<term1>)\}
        { return { access_hash_element => $() } }
RULE
*{'assign_hash_to_scalar'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<variable>:=(<varscalar>)<?p6ws>?\=<?p6ws>?$<value>:=(<varhash>)<?p6ws>?\;
        { return { assign_hash_to_scalar => $() } }
RULE
*{'assign_slurp_to_variable'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<variable>:=(<variable>)<?p6ws>?\=<?p6ws>?slurp<?p6ws>?$<value>:=(<term1>)<?p6ws>?\;
        { return { slurp => $() } }
RULE
*{'assign_open_to_variable'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<variable>:=(<variable>)<?p6ws>?\=<?p6ws>?open<?p6ws>?$<value>:=(<term1>)<?p6ws>?\;
        { return { _open => $() } }
RULE
*{'assign'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<variable>:=(<term1>)<?p6ws>?\=<?p6ws>?$<value>:=(<term1>)<?p6ws>?\;
	{ return { assign => $() } }
RULE
*{'sub_call'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<name>:=(<ident>)<?p6ws>?\(<?p6ws>?$<params>:=(<list>?)<?p6ws>?\)<?p6ws>?\;
        { return { sub_call => $() } }
RULE
*{'_push'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<op> := (push|unshift) <p6ws> <variable> <p6ws>? \, <p6ws>?
    $<code> := (.*?) <p6ws>? \;
        { return { _push => $() ,} }
RULE
*{'pod'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();
 
    \=[pod|head1|kwid|for] 
    .*? 
    \=cut 
RULE
*{'use_v6'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();
 
    use <?p6ws> v6 \-pugs <?p6ws>? \;
RULE
*{'require'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    require <?p6ws> <ident> <?p6ws>? \;
        { 
		eval 'require '.$()->[2]{ident}[0]{ident};
		return { require_bareword => $() ,} 
	}
RULE
*{'use_rule'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    use <?p6ws> <ident> <?p6ws>? \;
        { 
		eval 'use '.$()->[2]{ident}[0]{ident};
		return { use_bareword => $() ,} 
	}
RULE
*{'term1'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    <@Grammar::Perl6::terms>
RULE
*{'list'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    [ <term1> <?p6ws>? \, <?p6ws>? ]* <term1>?
RULE
*{'block'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    \{ 
        $<list> := ( [ <?p6ws>? <@Grammar::Perl6::statements> ]* ) <?p6ws>? 
    \}
        { return { block => $<list> ,} }
RULE
*{'macro_decl'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    macro <?p6ws> $<prefix> := (<word>) \: \< $<id> := (.*?) \> <?p6ws>? 
    \(  <?p6ws>? <list>? <?p6ws>? \) <?p6ws>?
    is <?p6ws> parsed <?p6ws>? \( 
        <?p6ws>? \/ <?p6ws>? <rule> <?p6ws>? \/ <?p6ws>? 
        \) <?p6ws>?
    <code> 
        {
	 eval Emitter::Perl5::emit({macro => $()});
	 return { macro => $() ,}
	}
RULE
*{'empty_list'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    \(\)
        { return { empty_list => $() } }
RULE
*{'_open'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

	$<op> := (open) <p6ws> <varscalar> <p6ws>? \;
		{ return { _open => $(), } }
RULE
*{'_print_with_fh'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();
 
    $<op> := (print|say|warn|die) <p6ws> <indirect_object> <p6ws> <list> <p6ws>? \;
        { return { _print_with_fh => $() ,} }
RULE
*{'_print'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();
 
    $<op> := (print|say|warn|die) <p6ws> <list> <p6ws>? \;
        { return { _print => $() ,} }
RULE
*{'_my'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<op> := (my|our|local) <p6ws> <variable> <p6ws>? \;
        { return { _my => $() ,} }
RULE
*{'_simple_statement'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<op> := (die|\.\.\.) \;
        { return { _simple_statement => $() ,} }
RULE
*{'sub_decl'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    sub <?p6ws> $<fix> := (infix|prefix|postfix) 
        \: \< $<id> := (.*?) \> <?p6ws>? <block>
        { return { sub_decl => $() ,} }
RULE
*{'sub_defin'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    sub <?p6ws>? <ident> <?p6ws>? <block>
        { return { sub_defin => $() ,} }
RULE
*{'term2'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<term1> := (<term1>) <p6ws>? 
    $<op>    := (<@Grammar::Perl6::ops>) <p6ws>? 
    $<term2> := (<term1>) 
        { return { sub_application_term => $() ,} }
RULE
*{'sub_application'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    $<term1> := (<term1>|<term2>) <p6ws>? 
    $<op>    := (<@Grammar::Perl6::ops>) <p6ws>? 
    $<term2> := (<term1>|<term2>) <p6ws>? \;
        { return { sub_application => $() ,} }
RULE
*{'eval_perl5'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    eval <p6ws>? \( <p6ws>? 
        <literal> <p6ws>? \, <p6ws>? 
        \: lang \< perl5 \> <p6ws>? 
    \) <p6ws>? \;
        { return { eval_perl5 => $<literal> } }
RULE
*{'_return'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();

    return <?p6ws> $<val> := (<term1>|<term2>) <?p6ws>? \;
        { return { _return => $() ,} }
RULE
push @terms, sub { Grammar::Perl6->indirect_object(@_) };
push @statements, sub { Grammar::Perl6->rule_decl(@_) };
push @statements, sub { Grammar::Perl6->grammar_name(@_) };
push @statements, sub { Grammar::Perl6->condition_rule(@_) };
push @statements, sub { Grammar::Perl6->meth_call_statement(@_) };
push @terms, sub { Grammar::Perl6->meth_call_term(@_) };
push @statements, sub { Grammar::Perl6->sub_call_statement(@_) };
push @terms, sub { Grammar::Perl6->sub_call_term(@_) };
push @terms, sub { Grammar::Perl6->access_hashref_element(@_) };
push @statements, sub { Grammar::Perl6->access_hashref_element(@_) };
push @terms, sub { Grammar::Perl6->access_hash_element(@_) };
push @statements, sub { Grammar::Perl6->access_hash_element(@_) };
push @statements, sub { Grammar::Perl6->assign_hash_to_scalar(@_) };
push @statements, sub { Grammar::Perl6->assign_slurp_to_variable(@_) };
push @statements, sub { Grammar::Perl6->assign_open_to_variable(@_) };
push @statements, sub { Grammar::Perl6->assign(@_) };
push @statements, sub { Grammar::Perl6->sub_call(@_) };
push @terms, sub { Grammar::Perl6->sub_call(@_) };
push @statements, sub { Grammar::Perl6->_push(@_) };
push @statements, sub { Grammar::Perl6->pod(@_) };
push @statements, sub { Grammar::Perl6->use_v6(@_) };
push @statements, sub { Grammar::Perl6->require(@_) };
push @statements, sub { Grammar::Perl6->use_rule(@_) };
push @statements, sub { Grammar::Perl6->block(@_) };
push @statements, sub { Grammar::Perl6->macro_decl(@_) };
push @terms, sub { Grammar::Perl6->empty_list(@_) };
push @terms, sub { Grammar::Perl6->varhash(@_) };
push @terms, sub { Grammar::Perl6->varscalar(@_) };
push @terms, sub { Grammar::Perl6->variable(@_) };
push @terms, sub { Grammar::Perl6->literal(@_) };
push @statements, sub { Grammar::Perl6->_open(@_) };
push @terms, sub { Grammar::Perl6->_open(@_) };
push @statements, sub { Grammar::Perl6->_print_with_fh(@_) };
push @statements, sub { Grammar::Perl6->_print(@_) };
push @statements, sub { Grammar::Perl6->_my(@_) };
push @statements, sub { Grammar::Perl6->_simple_statement(@_) };
push @statements, sub { Grammar::Perl6->sub_decl(@_) };
push @statements, sub { Grammar::Perl6->sub_defin(@_) };
push @statements, sub { Grammar::Perl6->sub_application(@_) };
push @statements, sub { Grammar::Perl6->eval_perl5(@_) };
push @statements, sub { Grammar::Perl6->_return(@_) };
1;
