use v5.10;
use MooseX::Declare;
use utf8;
class VAST::postcircumfix__S_Lt_Gt {
    use AST::Helpers;
    method emit_m0ld {
        my $key = $self->{'.'}[1]{'.'}{nibble}{'.'}[0]{TEXT};
        call 'postcircumfix:{ }'=>FETCH($self->{arg}->emit_m0ld),[string $key];
    }
}
