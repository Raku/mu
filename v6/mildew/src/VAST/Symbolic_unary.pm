package VAST::Symbolic_unary;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{SYM} eq '|' && $m->{arg}) {
	FETCH($m->{arg}->emit_m0ld);
    } elsif ($m->{SYM} eq '=' && $m->{arg}) {
	call('prefix:=' => FETCH($m->{arg}->emit_m0ld));
    } else {
	XXX;
    }
}

1;
