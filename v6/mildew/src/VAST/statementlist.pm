package VAST::statementlist;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    [map {$_->emit_m0ld} move_CONTROL($m->{statement})]
}

1;
