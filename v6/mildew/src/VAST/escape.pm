package VAST::escape;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{variable}) {
        $m->{variable}->emit_m0ld;
    } elsif ($m->{item}) {
        string $m->{item}->as_constant_string;
    } else {
        XXX;
    }
}

1;
