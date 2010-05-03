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
        my ($pos,$named) = named_and_positional(@{$methodop->{args}} ? $methodop->{args}[0]->emit_m0ld : ());
        if ($self->{'.'}[1]{SYM} eq '.^!') {
            $methodname = '^!' . $methodname;
        } elsif ($self->{'.'}[1]{SYM} eq '.^') {
            return let $self->{arg}->emit_m0ld => sub {
                my $obj = shift;
                call $methodname => FETCH(call('^!how' => FETCH($obj))),[$obj,@{$pos}],$named;
            }
        } else {
            XXX;
        }
        call $methodname => FETCH($self->{arg}->emit_m0ld),$pos,$named;
    }
}
