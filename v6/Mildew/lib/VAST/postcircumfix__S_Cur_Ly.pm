use v5.10;
use MooseX::Declare;
use utf8;
class VAST::postcircumfix__S_Cur_Ly {
    use Mildew::AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        call 'postcircumfix:{ }'=>FETCH($self->{arg}->emit_m0ld),[$self->{postop}{postcircumfix}{semilist}{statement}[0]->emit_m0ld];
    }
}
