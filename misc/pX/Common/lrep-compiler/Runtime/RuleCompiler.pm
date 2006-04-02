package Runtime::RuleCompiler;
use Pugs::Emitter::Rule::Perl5;
use base qw(Pugs::Compiler::Rule);
use Exporter 'import';
use Data::Dumper;
use strict;
our @EXPORT_OK = qw(rule);

sub compiled {
    my $class = shift;
    my $code = shift;
    my ($grammar) = caller;
    bless { code => $code, grammar => $grammar }, $class;
}

sub rule(&) {
    my $rule = shift;
    return Runtime::RuleCompiler->compiled(sub {
       	my $grammar = shift;
	my $tree;
	print Dumper($rule->($grammar)->( $_[0], undef, $tree, $tree )) if $ENV{DEBUG_lrep};
	Pugs::Runtime::Rule::rule_wrapper($_[0],$rule->($grammar)->( $_[0], undef, $tree, $tree )); 
    })->code();
}
sub perl5 {
    my $self = shift;
    my $ast = $self->{ast}{capture};
    Dumper($ast);
    $_ = q[rule {
    package Pugs::Runtime::Rule;
    my $grammar = shift;
    ].Pugs::Emitter::Rule::Perl5::emit_rule( $ast, '    ' )."}";
    s/\$_\[4\]/\$grammar/g;
    return $_;
}

1;
