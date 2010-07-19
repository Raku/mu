use v5.10;
use MooseX::Declare;
class Mildew::AST::Branch extends Mildew::AST::Base {
    has 'cond' => (is=>'ro');
    has 'then' => (is=>'rw');
    has 'else' => (is=>'rw');
    method pretty {
        "if "
        . $self->cond->pretty
        . " {goto " . $self->then->id . "} else {goto " . $self->else->id . "}";
    }
    method m0ld($target) {
        "if "
        . $self->cond->m0ld_literal
        . " {goto " . $self->then->id . "} else {goto " . $self->else->id . "};\n";
    }
    method simplified {
        my ($cond,@setup) = $self->cond->simplified;
        (Mildew::AST::Branch->new(cond=>$cond,then=>$self->then,else=>$self->else,@setup));
    }
}
