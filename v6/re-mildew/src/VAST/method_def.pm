package VAST::method_def;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    AST::Let->new(value => FETCH(lookup('$?CLASS')), block => sub {
        my $CLASS = shift;
        use YAML::XS;
        call add_method => FETCH(call '^!how' => $CLASS),[$CLASS,string $m->{longname}->canonical, routine($m->{blockoid},$m->{multisig}[0]{signature}[0])];
    });
}

1;
