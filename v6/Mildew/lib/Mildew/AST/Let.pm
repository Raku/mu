use v5.10;
use MooseX::Declare;
class Mildew::AST::Let extends Mildew::AST::Base {
    has 'block' => (is=>'ro');
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        my $id = Mildew::AST::unique_id;
        $self->value->m0ld($id) . $self->block->(Mildew::AST::Reg->new(name=>$id))->m0ld($ret);
    }
    method pretty {
        my $id = Mildew::AST::unique_id;
        "do {\n". Mildew::AST::indent('my ' . $id . ' = ' . $self->value->pretty . ";\n"
        . $self->block->(Mildew::AST::Reg->new(name => $id))->pretty) . '}';
    }
    method simplified {
        my ($value,@value_setup) = $self->value->simplified;
        my ($ret,@setup) = $self->block->($value)->simplified;
        ($ret,@value_setup,@setup);
    }
}
