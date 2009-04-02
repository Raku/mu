package VAST::quote;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    # XXX escapes
    my @things;
    for (@{$m->{nibble}{nibbles}}) {
        if (ref $_) {
            push(@things,$_->emit_m0ld);
        } elsif ($_) {
            push(@things,string $_);
        }
    }
    if (scalar @things == 1) {
        $things[0];
    } elsif (scalar @things) {
        fcall '&infix:~',[@things];
    } else {
        string '';
    }
}

1;
