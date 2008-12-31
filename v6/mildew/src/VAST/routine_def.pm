package VAST::routine_def;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    routine($m->{block},$m->{multisig}[0]{signature}[0]);
}

1;
