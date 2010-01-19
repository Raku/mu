use v5.10;
use MooseX::Declare;
class AST::Let extends AST::Base {
    has 'block' => (is=>'ro');
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        my $id = AST::unique_id;
        $self->value->m0ld($id) . $self->block->(AST::Reg->new(name=>$id))->m0ld($ret);
    }
    method pretty {
        my $id = AST::unique_id;
        "do {\n". AST::indent('my ' . $id . ' = ' . $self->value->pretty . ";\n"
        . $self->block->(AST::Reg->new(name => $id))->pretty) . '}';
    }
    method simplified {
        my ($value,@value_setup) = $self->value->simplified;
        my ($ret,@setup) = $self->block->($value)->simplified;
        ($ret,@value_setup,@setup);
    }
}
