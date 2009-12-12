use v5.10;
use MooseX::Declare;
use utf8;
class VAST::postcircumfix__S_Paren_Thesis {
    use AST::Helpers;
    method emit_m0ld {
        call 'postcircumfix:( )' => FETCH($self->{arg}->emit_m0ld),[capturize(named_and_positional($self->{postop}{postcircumfix}{semiarglist}->emit_m0ld))];
    }
}
