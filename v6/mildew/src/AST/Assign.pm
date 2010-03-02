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
        my $node = $self->pretty;
        $node .= " - ".sprintf("%.4f",$Mildew::took->{$self->id}) if defined $Mildew::took && $Mildew::took->{$self->id};
        Forest::Tree->new(node=>$node,children=>[$self->lvalue->forest,$self->rvalue->forest]);
    }
}
