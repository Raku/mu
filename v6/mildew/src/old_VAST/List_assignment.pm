package VAST::List_assignment;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{infix}{TEXT} eq '=') {
        AST::Call->new(
            identifier=>string 'STORE',
            capture=>AST::Capture->new(invocant=>$m->{left}->emit_m0ld,positional=>[FETCH($m->{right}->emit_m0ld)]),
        );
    } else {
        XXX;
    }
}

1;
