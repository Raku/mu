use v5.10;
use MooseX::Declare;
class Mildew::AST::StringConstant extends Mildew::AST::Base {;
    has 'value' => (is=>'ro');
    has 'type_info' => (is=>'ro',lazy=>1,default=>sub {Mildew::TypeInfo::StringConstant->new()});
    method m0ld($ret) {
        #XXX metachars
        "my $ret = \"".$self->m0ld_literal."\";\n";
    }
    method pretty {
        #XXX metachars
        '"' . $self->value . '"'
    }
    method simplified {
        $self;
    }
    method m0ld_literal {
        my $str = $self->value;
        $str =~ s/\\/\\\\/g;
        $str =~ s/"/\\"/g;
        $str =~ s/\n/\\n/g;
        $str;
    }
}
