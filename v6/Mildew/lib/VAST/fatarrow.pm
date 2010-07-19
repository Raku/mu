package VAST::fatarrow;
use utf8;
use strict;
use warnings;
use Mildew::AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    Mildew::AST::Pair->new(key => $m->{key}->emit_m0ld, value => $m->{val}->emit_m0ld);
}

1;
