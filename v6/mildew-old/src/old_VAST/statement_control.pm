package VAST::statement_control;
use utf8;
use strict;
use warnings;
use AST::Helpers;
use Scalar::Util qw(blessed);

sub emit_m0ld {
    my $m = shift;
    if ($m->{sym} eq 'unless') {
        my $then = call 'postcircumfix:( )' => code($m->{xblock}{pblock}{blockoid}),[capturize];
        AST::If->new
            ( cond => $m->{xblock}{EXPR}->emit_m0ld,
              else => $then )

    } elsif ($m->{sym} eq 'if') {
        my $then = call 'postcircumfix:( )' => code($m->{xblock}{pblock}{blockoid}),[capturize];
        my $else = lookupf("False");
        if (ref $m->{else} eq 'ARRAY' &&
            blessed $m->{else}[0] &&
            ref $m->{else}[0]{pblock} &&
            ref $m->{else}[0]{pblock}{blockoid}) {

            $else = call 'postcircumfix:( )' => code($m->{else}[0]{pblock}{blockoid}),[capturize];
        }

        my @elsif;
        if (ref $m->{elsif} eq 'ARRAY') {
            foreach my $elsif_part (@{$m->{elsif}}) {

                my $elsif = call 'postcircumfix:( )' => code($elsif_part->{xblock}{pblock}{blockoid}),[capturize];

                push @elsif, AST::If->new
                  ( cond => $elsif_part->{xblock}{EXPR}->emit_m0ld,
                    then => $elsif );
            }
        }

        AST::If->new
            ( cond => $m->{xblock}{EXPR}->emit_m0ld,
              then => $then,
              else => $else,
              elsif => \@elsif );

    } elsif ($m->{sym} eq 'CONTROL') {
        # CONTROL blocks are moved to the top of the
        # statementlist, so we know that no code was executed
        # before this, so we can peacefully delay the setup of the
        # control block up to this point.
        call 'set_control' => (call 'continuation' => reg '$interpreter'), [ code($m->{block},FETCH(lookup('$DefaultBlockSignature'))) ];
    } elsif ($m->{sym} eq 'CATCH') {
        # the same for CATCH blocks.
        call 'set_catch' => (call 'continuation' => reg '$interpreter'), [ code($m->{block},FETCH(lookup('$DefaultBlockSignature'))) ];

    } elsif ($m->{sym} eq 'loop') {
        AST::Loop->new(code => call('postcircumfix:( )',code($m->{block}),[capturize([])]));

    } elsif ($m->{sym} eq 'while') {
        AST::While->new(cond => EXPR($m->{xblock}{EXPR}), body => call('postcircumfix:( )',code($m->{xblock}{pblock}{blockoid}),[capturize([])]));

    } elsif ($m->{sym} eq 'until') {
        AST::While->new(cond => fcall('&not' => [EXPR($m->{xblock}{EXPR})]), body => call('postcircumfix:( )',code($m->{xblock}{pblock}{blockoid}),[capturize([])]));

    } elsif ($m->{sym} eq 'for') {
        fcall '&map' => [code($m->{xblock}{pblock}{blockoid},$m->{xblock}{pblock}{signature}->emit_m0ld),EXPR($m->{xblock}{EXPR})];

    } elsif ($m->{sym} eq 'use') {
    } else {
        XXX('unkown sym in statement_control')
    }
}


1;
