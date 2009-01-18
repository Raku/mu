package VAST::colonpair;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    my $key;
    if (! ref $m->{k}) {
        $key = string $m->{k};
    } else {
        XXX;
    }

    my $value;
    if (ref $m->{v} eq 'VAST::postcircumfix') {
        if ($m->{v}{sym}[0] eq '(' &&
            $m->{v}{sym}[1] eq ')') {
            $value = $m->{v}{semilist}{statement}[0]->emit_m0ld;
        } elsif ($m->{v}{sym}[0] eq '<' &&
                 $m->{v}{sym}[1] eq '>') {
            $value = $m->{v}{nibble}->emit_m0ld;
        } else {
            XXX;
        }
    } else {
        XXX
    }
    AST::Pair->new(key => $key, value => $value);
}

1;
