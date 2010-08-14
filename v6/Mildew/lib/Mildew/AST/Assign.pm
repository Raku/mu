use v5.10;
use MooseX::Declare;
class Mildew::AST::Assign extends Mildew::AST::Base {
    use Mildew::Emit::Haskell;
    has 'lvalue' => (is => 'ro');
    has 'rvalue' => (is => 'ro');
    method pretty {
       $self->lvalue->pretty . " = " . $self->rvalue->pretty;
    }
    method m0ld($target) {
        $self->rvalue->m0ld($self->lvalue->m0ld_literal);
    }
    method took {
        $Mildew::took->{$self->id};
    }
    method simplified {
        my ($rvalue,@setup) = $self->rvalue->simplified;
        ($self->lvalue,@setup,Mildew::AST::Assign->new(lvalue=>$self->lvalue,rvalue=>$rvalue));
    }
    method haskell_literal {
        constructor('Assign',$self->lvalue,$self->rvalue);
    }
    method forest {
        my $node = $self->pretty;
        $node .= " - ".sprintf("%.4f",$self->took) if defined $Mildew::took && $Mildew::took->{$self->id};
        Forest::Tree->new(node=>$node,children=>[$self->lvalue->forest,$self->rvalue->forest]);
    }

}

=head1 NAME

Mildew::AST::Assign - an assignment to a register

=head1 DESCRIPTION

Assign a value or a result of a call to a register.

=head1 ATTRIBUTES

=over 4

=item rvalue

The thing which we will assign to lvalue.
In simplified AST it is a constant,Mildew::AST::Reg or Mildew::AST::Call.

=item lvalue

Mildew::AST::Reg which will be assigned to

=item then

Where to jump if I<cond> is not equal to native false

=cut
