package VAST::Additive;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{infix}) {
        fcall '&infix:'.$m->{infix}{TEXT},[$m->{left}->emit_m0ld,$m->{right}->emit_m0ld];
    } else {
        use YAML::XS;
        die Dump($m);
        XXX;
    }
}

1;
