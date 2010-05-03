use v5.10;
use MooseX::Declare;
class AST::Comment extends AST::Base {
    has 'comment' => (is=>'ro');
    method m0ld($ret) {
        join("",map {"#".$_."\n"} split(/\n/,$self->comment));
    }
}
