package VAST::Terminator;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{noun}) {
        $m->{noun}->emit_m0ld;
    } else {
        XXX;
    }
}

1;
