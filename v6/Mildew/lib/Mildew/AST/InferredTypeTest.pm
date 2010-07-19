use v5.10;
use MooseX::Declare;
class Mildew::AST::InferredTypeTest extends Mildew::AST::Base {
    has 'value' => (is=>'ro');
    has 'test' => (is=>'ro');
    method simplified {
        my ($value,@setup) = $self->value->simplified;
        my $ret = Mildew::AST::unique_reg;
        ($ret,@setup,Mildew::AST::Assign->new(lvalue=>$ret,rvalue=>Mildew::AST::InferredTypeTest->new(value=>$value,test=>$self->test)));
    }
    method m0ld {
        die "INFERRED-TYPE-CHECK is only suported on the optC backend\n";
    }
    method pretty {
        'INFERRED-TYPE-CHECK('.$self->value->pretty.','.$self->test.')'; 
    }
    method forest {
        Forest::Tree->new(node=>$self->pretty,children=>[$self->value->forest]);
    }

}
