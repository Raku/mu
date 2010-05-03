use v5.10;
use MooseX::Declare;
class VAST::prefix__S_PlusPlus {
    use AST::Helpers;
    method emit_m0ld {
        #XXX should be prefix
        fcall '&prefix:++' => [$self->{arg}->emit_m0ld];
    }
}
