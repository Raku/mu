use v5.10;
use MooseX::Declare;
class VAST::infix__S_Equal {
    use AST::Helpers;
    method emit_m0ld {
        call 'STORE' => $self->{args}[0]->emit_m0ld,[FETCH($self->{args}[1]->emit_m0ld)];
    }
}
