package VAST::comp_unit;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my ($m,$C) = @_;
    # we insert reg '$scope' so that modules can return their outermost scope
    AST::Block->new(regs=>['interpreter','scope'],stmts=>trailing_return([@{$m->{statementlist}->emit_m0ld},defined $Mildew::return_real_value ? () : reg '$scope' ]));
}

1;
