package VAST::blockoid;
use utf8;
use strict;
use warnings;
use Mildew::AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    local $Mildew::multis;
    Mildew::AST::Block->new(regs=>['interpreter','scope'],stmts=>trailing_return($m->{statementlist}->emit_m0ld));
}

1;
