use v5.10;
use utf8;
use MooseX::Declare;
class Mildew::AST::Phi extends Mildew::AST::Base {
    has 'regs' => (is => 'ro');
    method pretty {
        "phi(".join (',',map {$_->pretty} @{$self->regs}).")";
    }
    method m0ld($target) {
        die "phi functions can't be represented in mold";
    }
}
