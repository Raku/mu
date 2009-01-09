package VAST::Concatenation;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{infix}{TEXT} eq '~') {
        fcall '&infix:~',[$m->{left}->emit_m0ld,$m->{right}->emit_m0ld];
    } else {
        XXX;
    }
}

1;
