package VAST::fatarrow;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    AST::Pair->new(key => $m->{key}->emit_m0ld, value => $m->{val}->emit_m0ld);
}

1;
