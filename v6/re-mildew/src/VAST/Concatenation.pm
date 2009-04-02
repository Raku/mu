package VAST::Concatenation;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{sym} eq '~') {
        fcall '&infix:~',[map {$_->emit_m0ld} @{$m->{list}}];
    } else {
        use YAML::XS;
        die Dump($m);
        XXX;
    }
}

1;
