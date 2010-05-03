package VAST::statement_prefix;
use utf8;
use strict;
use warnings;
use AST::Helpers;
use Scalar::Util qw(blessed);

sub emit_m0ld {
    my $m = shift;
    if ($m->{sym} eq 'do') {
        $m->{statement}->emit_m0ld;
    } else {
        die $m->{sym};
        XXX('unkown sym in statement_prefix')
    }
}


1;
