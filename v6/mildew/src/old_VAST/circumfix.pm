package VAST::circumfix;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{sym}[0] eq '{' && $m->{sym}[1] eq '}' && scalar @{$m->{sym}} == 2) {
        code($m->{pblock}{blockoid});
    } elsif ($m->{sym}[0] eq '(' && $m->{sym}[1] eq ')' && scalar @{$m->{sym}} == 2) {
	$m->{semilist}{statement}[0]->emit_m0ld;
    } else {
        XXX;
    }
}

1;
