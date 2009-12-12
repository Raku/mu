use v5.10;
use MooseX::Declare;
class VAST::dotty__S_Dot {
    use AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        # STD workaround
        my $methodop = $self->{'.'}[1]{dotty}{dottyop}{methodop};
        call $methodop->{longname}->canonical => FETCH($self->{arg}->emit_m0ld),named_and_positional(
            @{$methodop->{args}} ? $methodop->{args}[0]->emit_m0ld : ()
        )
    }
}
