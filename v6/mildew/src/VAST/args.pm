package VAST::args;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    $m->{semiarglist} ? $m->{semiarglist}->emit_m0ld : ();
}

1;
