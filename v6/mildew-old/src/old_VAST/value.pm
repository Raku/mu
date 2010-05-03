package VAST::value;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{number}) {
        $m->{number}->emit_m0ld;
    } elsif ($m->{quote}) {
        $m->{quote}->emit_m0ld;
    } else {
        warn Dump($m);
        XXX;
    }
}

1;
