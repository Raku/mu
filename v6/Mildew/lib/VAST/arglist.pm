package VAST::arglist;
use utf8;
use strict;
use warnings;
use Mildew::AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    $m->{EXPR} ? $m->{EXPR}->emit_m0ld : ();
}

1;
