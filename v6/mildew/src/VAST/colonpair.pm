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
        if ($m->{v}{SYM}[0] eq '(' &&
            $m->{v}{SYM}[1] eq ')') {
            $value = $m->{v}{semiarglist}->emit_m0ld;
        } elsif ($m->{v}{SYM}[0] eq '<' &&
                 $m->{v}{SYM}[1] eq '>') {
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
