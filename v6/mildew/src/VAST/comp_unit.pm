package VAST::comp_unit;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my ($m) = @_;
    AST::Block->new(regs=>['interpreter','scope'],stmts=>$m->{statementlist}->emit_m0ld);
}

1;
