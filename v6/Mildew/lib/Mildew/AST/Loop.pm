use v5.10;
use MooseX::Declare;
class Mildew::AST::Loop extends Mildew::AST::Base {
    has 'code' => (is => 'ro');
    method m0ld($ret) {
        my $label = Mildew::AST::unique_label;
        $label.':'.($self->code->m0ld($ret))."\n".
        'goto '.$label.';'."\n";
    }
    method simplified {
        my $goto = Mildew::AST::Goto->new();
        my ($ret,@setup) = $self->code->simplified;
        my $block = Mildew::AST::Seq->new(id=>Mildew::AST::unique_label,stmts=>[@setup,$goto]);
        $goto->block($block);
        ($ret,$block);
    }
    method pretty {
        return "loop {\n"
        . Mildew::AST::indent($self->code->pretty) . "\n"
        . "}\n";
    }
}
