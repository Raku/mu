package VAST::args;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{semiarglist}) {
        $m->{semiarglist}->emit_m0ld;
    } elsif ($m->{arglist}[0]) {
        $m->{arglist}[0]->emit_m0ld;
    } else {
        #???
    }
}

1;
