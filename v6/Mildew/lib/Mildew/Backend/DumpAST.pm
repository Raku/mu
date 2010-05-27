use v5.10;
use MooseX::Declare;
class Mildew::Backend::DumpAST with Mildew::Backend {
    has format=>(is=>'ro');
    method compile($ast,$output) {
        $self->output($self->format->($ast),$output);
    }
    method run($ast) {
        die;
    }
}
__END__
=pod 

=head1 NAME

Mildew::Backend::DumpAST

=head1 DESCRIPTION

This backend prints out the AST in a format specified by the closure.

=cut
