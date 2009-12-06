use v5.10;
use MooseX::Declare;
class VAST::Chaining {
    use AST::Helpers;
    method emit_m0ld {
        # chainging
        if (@{$self->{chain}} != 3) {
            XXX;
        }
        fcall '&infix:'.$self->{chain}[1]{SYM},[$self->{chain}[0]->emit_m0ld,$self->{chain}[2]->emit_m0ld];
    }
}
