package VAST::colonpair;
use utf8;
use strict;
use warnings;
use v5.10;
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
    if (ref $m->{v}{circumfix} eq 'VAST::circumfix__S_Paren_Thesis') {
        $value = $m->{v}{circumfix}{semilist}{statement}[0]->emit_m0ld;
    } elsif (ref $m->{v}{circumfix} eq 'VAST::circumfix__S_Lt_Gt') {
        $value = $m->{v}{circumfix}{nibble}->emit_m0ld;
    } else {
        XXX
    }
    AST::Pair->new(key => $key, value => $value);
}

1;
