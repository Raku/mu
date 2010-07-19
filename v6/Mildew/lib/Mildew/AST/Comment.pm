use v5.10;
use MooseX::Declare;
class Mildew::AST::Comment extends Mildew::AST::Base {
    has 'comment' => (is=>'ro');
    method m0ld($ret) {
        join("",map {"#".$_."\n"} split(/\n/,$self->comment));
    }
}
