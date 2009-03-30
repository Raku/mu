package VAST::Autoincrement;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{SYM} eq '++') {
        fcall '&postfix:++',[$m->{arg}{noun}->emit_m0ld];
    } else {
        XXX;
    }
}

1;
