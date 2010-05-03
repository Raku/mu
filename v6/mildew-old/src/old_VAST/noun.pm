package VAST::noun;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    if ($m->{variable}) {
        $m->{variable}->emit_m0ld;
    } elsif ($m->{value}) {
        $m->{value}->emit_m0ld;
    } elsif ($m->{routine_declarator}) {
        if ($m->{routine_declarator}{routine_def}) {
            $m->{routine_declarator}{routine_def}->emit_m0ld;
        } elsif ($m->{routine_declarator}{method_def}) {
            $m->{routine_declarator}{method_def}->emit_m0ld;
        } else {
            XXX;
        }
    } elsif ($m->{term}) {
        $m->{term}->emit_m0ld;
    } elsif ($m->{multi_declarator}) {
        $m->{multi_declarator}->emit_m0ld;
    } elsif ($m->{scope_declarator}) {
        $m->{scope_declarator}->emit_m0ld;
    } elsif ($m->{package_declarator}) {
        $m->{package_declarator}->emit_m0ld;
        #my $p = $m->{package_declarator};
    } elsif ($m->{statement_prefix}) {
        $m->{statement_prefix}->emit_m0ld;
    } elsif ($m->{circumfix}) {
        $m->{circumfix}->emit_m0ld;
    } elsif ($m->{sigterm}) {
        $m->{sigterm}->emit_m0ld;
    } elsif ($m->{colonpair}) {
        if ($m->{fatarrow}) {
            $m->{fatarrow}->emit_m0ld;
        } else {
            $m->{colonpair}[0]->emit_m0ld;
        }
    } else {
        XXX;
    }
}

1;
