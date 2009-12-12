use v5.10;
use MooseX::Declare;
class VAST::dotty__S_Dot {
    use AST::Helpers;
    method emit_m0ld {
        use YAML::XS;
        # STD workaround

        my $dottyop = $self->{'.'}[1]{dotty}{dottyop};
        if (my $methodop = $dottyop->{methodop}) {
            call $methodop->{longname}->canonical => FETCH($self->{arg}->emit_m0ld),named_and_positional(
                @{$methodop->{args}} ? $methodop->{args}[0]->emit_m0ld : ()
            )
        } elsif (my $postcircumfix = $dottyop->{postop}{postcircumfix}) {
            call 'postcircumfix:( )' => FETCH($self->{arg}->emit_m0ld),[capturize(named_and_positional($postcircumfix->{semiarglist}->emit_m0ld))];
        } else {
            Mildew::prune($dottyop);
            die Dump($dottyop);
        }
    }
}
