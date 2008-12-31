package VAST::nulltermish;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    $m->{termish}[0] ? $m->{termish}[0]->emit_m0ld : ();
}

1;
