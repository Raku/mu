use v5.10;
use MooseX::Declare;
use utf8;
class Mildew::AST::Block::Simplified extends Mildew::AST::Block {
    use Mildew::Emit::Haskell;
    method simplified {
        $self;
    }
    method haskell_literal {
        constructor('Block',$self->stmts,$self->regs);
    }
}
