use v5.10;
use MooseX::Declare;
class AST::Reg extends AST::Base {
    has 'name' => (is=>'ro');
    has 'real_name' => (is=>'ro');
    has 'type_info' => (is=>'rw');
    method m0ld($ret) {
        "my $ret = ".$self->name.";\n";
    }
    method pretty {
        #XXX metachars
        $self->name;
    }
    method simplified {
        $self;
    }
    method m0ld_literal {
        $self->name;
    }
    method forest {
        Forest::Tree->new(node=>$self->pretty,children=>[Forest::Tree->new(node=>$self->type_info->type->pretty)]);
    }
}
















