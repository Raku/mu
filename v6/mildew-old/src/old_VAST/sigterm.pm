package VAST::sigterm;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    $m->{signature}->emit_m0ld;
}

1;
