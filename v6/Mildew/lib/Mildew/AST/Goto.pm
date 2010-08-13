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

=head1 NAME

Mildew::AST::Goto - a unconditional jump

=head1 DESCRIPTION

Jumps to block

=head1 ATTRIBUTES

=over 4

=item block

The block to which we jump

=cut
