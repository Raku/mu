package Runtime::RuleCompiler;

use base qw(Pugs::Compiler::Rule);

sub compiled {
    my $class = shift;
    my $code = shift;
    my ($grammar) = caller;
    bless { code => $code, grammar => $grammar }, $class;
}

sub perl5 {
    my $self = shift;
    return $self->{perl5};
}


1;
