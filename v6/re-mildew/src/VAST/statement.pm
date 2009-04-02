package VAST::statement;
use utf8;
use strict;
use warnings;
use AST::Helpers;
use Scalar::Util qw(blessed);

sub emit_m0ld {
    my $m = shift;
    if ($m->{label}) {
        AST::Label->new(label=>label($m->{label}),stmt=>statement($m->{statement}));
    } elsif ($m->{statement_control}) {
        $m->{statement_control}->emit_m0ld;
    } elsif ($m->{EXPR}) {
        $m->{EXPR}->emit_m0ld;
    } else {
        XXX('unknown statement')
    }
}


1;
