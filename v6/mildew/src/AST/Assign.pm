use v5.10;
use MooseX::Declare;
class AST::Assign extends AST::Base {
    has 'lvalue' => (is => 'ro');
    has 'rvalue' => (is => 'ro');
    method pretty {
       $self->lvalue->pretty . " = " . $self->rvalue->pretty;
    }
}
