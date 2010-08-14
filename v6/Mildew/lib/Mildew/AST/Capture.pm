use v5.10;
use MooseX::Declare;
class Mildew::AST::Capture extends Mildew::AST::Base {
    use Mildew::Emit::Haskell;
    has 'invocant' => (is=>'ro',isa=>'Mildew::AST::Base');
    has 'positional' => (is=>'ro',default=>sub {[]},isa=>'ArrayRef[Mildew::AST::Base]');
    has 'named' => (is=>'ro',default=>sub {[]},isa=>'ArrayRef[Mildew::AST::Base]');
    method haskell_literal {
        constructor('Capture',$self->invocant,$self->positional,$self->named);
    }
}
