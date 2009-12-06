use v5.10;
use MooseX::Declare;
class VAST::postfix__S_PlusPlus {
    use AST::Helpers;
    method emit_m0ld {
        fcall '&postfix:++' => [$self->{arg}->emit_m0ld];
    }
}
