package VAST::args;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{listopargs}) {
        my $positional = $m->{semilist}{statement}[0];
        [(map {$_->{EXPR}->emit_m0ld} @{$m->{listopargs}}),($positional ? $positional->emit_m0ld : ())];
    } else {
        XXX;
    }
}

1;
