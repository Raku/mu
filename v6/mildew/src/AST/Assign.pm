use v5.10;
use MooseX::Declare;
class AST::Assign extends AST::Base {
    has 'lvalue' => (is => 'ro');
    has 'rvalue' => (is => 'ro');
    method pretty {
       $self->lvalue->pretty . " = " . $self->rvalue->pretty;
    }
    method m0ld($target) {
        $self->rvalue->m0ld($self->lvalue->m0ld_literal);
    }
    method forest {
        Forest::Tree->new(node=>$self->pretty,children=>[$self->lvalue->forest,$self->rvalue->forest]);
    }
}
