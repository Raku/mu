use v5.10;
use MooseX::Declare;
class AST::InferredTypeTest extends AST::Base {
    has 'value' => (is=>'ro');
    has 'test' => (is=>'ro');
    method simplified {
        my ($value,@setup) = $self->value->simplified;
        my $ret = AST::unique_reg;
        ($ret,@setup,AST::Assign->new(lvalue=>$ret,rvalue=>AST::InferredTypeTest->new(value=>$value,test=>$self->test)));
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
