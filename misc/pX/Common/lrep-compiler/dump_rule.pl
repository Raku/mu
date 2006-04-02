#!/usr/bin/perl

use Runtime::RuleCompiler;
use Perl6::Slurp;
my $filename = shift;
my $tail = slurp($filename);

print <<PRELUDE;
package Grammar::Perl6;
use base 'Pugs::Grammar::Base', 'Pugs::Grammar::Rule', 'Grammar::Perl6Init';
use Runtime::RuleCompiler qw(rule);
use Pugs::Grammar::Rule;
use Pugs::Runtime::Match;
PRELUDE

RULEMATCH:
while ($tail =~ m/rule\s+(\S+)\s+\{(.+)$/gs) {
    my $rulename = $1;
    $tail = $2;
    my $source = "";
    my $deep = 1;
  PARENMATCH:
    while ($tail =~ m/([^\{\}]*)([\{\}])(.+)$/gs) {
        $source .= $1;
        my $paren = $2;
        $tail = $3;
        if ($paren eq '}') {
            $deep--;
            last PARENMATCH unless $deep;
        } else {
            $deep++;
        }
        $source .= $paren;
    }

    #my $rule = Runtime::RuleCompiler->compile($source);
    #print '*{\''.$rulename.'\'} = '.$rule->perl5().";\n";
    print "*{\'$rulename\'} = Pugs::Compiler::Rule->compile(<<'RULE')->code();\n";
    print $source;
    print "RULE\n";
}
print <<'PUSH';
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
PUSH
print "1;\n";
__END__


package Test;
${'a'} = '10';
*{'a'} = sub { '20 ' };
print Test->a.$/;
print &Test::a.$/;
print $Test::a.$/;


${'rule'} = Pugs::Compiler::Rule->compiled(
sub {
    
}
});
*{'rule'} = sub {
    ${'rule'}->match(@_);
}
