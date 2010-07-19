use v5.10;
use MooseX::Declare;
use utf8;
class VAST::statement_control__S_for {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        fcall '&map' => [code($self->{xblock}{pblock}{blockoid},$self->{xblock}{pblock}{signature}->emit_m0ld),$self->{xblock}{EXPR}->emit_m0ld];
    }
}
