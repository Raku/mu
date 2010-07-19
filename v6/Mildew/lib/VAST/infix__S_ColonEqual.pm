use v5.10;
use MooseX::Declare;
class VAST::infix__S_ColonEqual {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        call BIND => $self->{args}[0]->emit_m0ld,[$self->{args}[1]->emit_m0ld];
    }
}
