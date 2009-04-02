package  VAST::numish;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{integer}) {
        $m->{integer}->emit_m0ld;
    } else {
        XXX;
    }
}

1;
