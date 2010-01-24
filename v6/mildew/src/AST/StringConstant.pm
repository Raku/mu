use v5.10;
use MooseX::Declare;
class AST::StringConstant extends AST::Base {;
    has 'value' => (is=>'ro');
    method m0ld($ret) {
        #XXX metachars
        my $str = $self->value;
        $str =~ s/\\/\\\\/g;
        $str =~ s/"/\\"/g;
        $str =~ s/\n/\\n/g;
        "my $ret = \"".$str."\";\n";
    }
    method pretty {
        #XXX metachars
        '"' . $self->value . '"'
    }
    method simplified {
        $self;
    }
}
