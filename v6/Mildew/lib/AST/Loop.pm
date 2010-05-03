use v5.10;
use MooseX::Declare;
class AST::Loop extends AST::Base {
    has 'code' => (is => 'ro');
    method m0ld($ret) {
        my $label = AST::unique_label;
        $label.':'.($self->code->m0ld($ret))."\n".
        'goto '.$label.';'."\n";
    }
    method simplified {
        my $goto = AST::Goto->new();
        my ($ret,@setup) = $self->code->simplified;
        my $block = AST::Seq->new(id=>AST::unique_label,stmts=>[@setup,$goto]);
        $goto->block($block);
        ($ret,$block);
    }
    method pretty {
        return "loop {\n"
        . AST::indent($self->code->pretty) . "\n"
        . "}\n";
    }
}
