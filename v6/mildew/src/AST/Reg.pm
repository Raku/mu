use v5.10;
use MooseX::Declare;
class AST::Reg extends AST::Base {
    has 'name' => (is=>'ro');
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
}
















