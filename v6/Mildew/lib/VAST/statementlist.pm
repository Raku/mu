package VAST::statementlist;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if (@{$m->{statement}}) {
        [map {$_->emit_m0ld} move_CONTROL($m->{statement})]
    } else {
        [lookup("False")]
    }
}

1;
