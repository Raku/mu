use v5.10;
use MooseX::Declare;
class VAST::dotty__S_DotStar {
    use AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        # STD workaround
        my $methodop = $self->{'.'}[1]{dotty}{dottyop}{methodop};
        my $methodname = $methodop->{longname}->canonical;
        #die Dump($self->{'.'}[1]{SYM});
        if ($self->{'.'}[1]{SYM} eq '.^!') {
            $methodname = '^!' . $methodname;
        } else {
            XXX;
        }
        my $args = @{$methodop->{args}} ? [$methodop->{args}[0]->emit_m0ld] : [];
        call $methodname => FETCH($self->{arg}->emit_m0ld),$args;
    }
}
