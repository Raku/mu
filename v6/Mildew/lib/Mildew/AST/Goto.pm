use v5.10;
use MooseX::Declare;
class Mildew::AST::Goto extends Mildew::AST::Base {
    has 'block' => (is => 'rw');
    method pretty {
        "goto ".$self->block->id;
    }
    method m0ld($target) {
        "goto ".$self->block->id.";\n";
    }
}
