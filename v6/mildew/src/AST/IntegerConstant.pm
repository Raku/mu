use v5.10;
use MooseX::Declare;
class AST::IntegerConstant extends AST::Base {
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        "my $ret = ".$self->value.";\n";
    }
    method pretty {
        $self->value
    }
    method simplified {
        $self;
    }
    method m0ld_literal {
        $self->value;
    }
 }
