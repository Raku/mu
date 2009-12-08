use v5.10;
use MooseX::Declare;
class VAST::dotty__S_Dot {
    use AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        # STD workaround
        my $methodop = $self->{'.'}[1]{dotty}{dottyop}{methodop};
        my $args = @{$methodop->{args}} ? [$methodop->{args}[0]->emit_m0ld] : [];
        call $methodop->{longname}->canonical => $self->{arg}->emit_m0ld,$args;
    }
}
