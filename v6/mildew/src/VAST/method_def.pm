package VAST::method_def;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    AST::Let->new(value => FETCH(lookup('$?CLASS')), block => sub {
        my $CLASS = shift;
        call add_method => FETCH(call '^!how' => $CLASS),[$CLASS,string $m->{longname}{name}{identifier}{TEXT}, routine($m->{block},$m->{multisig}[0]{signature}[0])];
    });
}

1;
