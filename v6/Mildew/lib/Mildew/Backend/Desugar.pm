use v5.10;
use MooseX::Declare;
class Mildew::Backend::Desugar with Mildew::Backend {
    method compile($ast,$output) {
        $self->output($ast->pretty."\n",$output);
    }
    method run($ast) {
        die;
    }
}
__END__
=pod 

=head1 NAME

Mildew::Backend::Desugar

=head1 DESCRIPTION

This backend prints out the AST in a pretty format.

=cut
