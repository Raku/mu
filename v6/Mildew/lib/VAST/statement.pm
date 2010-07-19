#XXX MooseX::Declare
package VAST::statement;
use utf8;
use strict;
use warnings;
use v5.10;
use Mildew::AST::Helpers;
use Scalar::Util qw(blessed);

sub emit_m0ld {
    my $m = shift;
    if ($m->{label}) {
        Mildew::AST::Label->new(label=>label($m->{label}),stmt=>statement($m->{statement}));
    } elsif ($m->{statement_control}) {
        $m->{statement_control}->emit_m0ld;
    } elsif ($m->{EXPR} && $m->{EXPR}{circumfix} && $m->{EXPR}{circumfix}->isa('VAST::circumfix__S_Cur_Ly')) {
        call 'postcircumfix:( )' => $m->{EXPR}->emit_m0ld,[capturize];
    } elsif ($m->{EXPR}) {
        $m->{EXPR}->emit_m0ld;
    } else {
        ();
    }
}


1;
