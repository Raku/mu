package VAST::comp_unit;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    #TODO refactor
    my ($m) = @_;
    # we insert reg '$scope' so that modules can return their outermost scope
    my $stmts = [@{$m->{statementlist}->emit_m0ld},defined $Mildew::return_real_value ? () : reg '$scope' ];
    AST::Block->new(regs=>['interpreter','scope'],stmts=>defined $Mildew::no_setr ? $stmts: trailing_return($stmts));
}

1;
