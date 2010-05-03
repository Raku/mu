use v5.10;
use MooseX::Declare;
class AST::Goto extends AST::Base {
    has 'block' => (is => 'rw');
    method pretty {
        "goto ".$self->block->id;
    }
    method m0ld($target) {
        "goto ".$self->block->id.";\n";
    }
}
