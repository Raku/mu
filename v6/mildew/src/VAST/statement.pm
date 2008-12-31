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
        my $stm = $m->{statement_control};

        if ($stm->{sym} eq 'if') {
            my $then = call 'postcircumfix:( )' => code($stm->{xblock}{pblock}{block}),[capturize];
            my $else;
            if (ref $stm->{else} eq 'ARRAY' &&
                blessed $stm->{else}[0] &&
                ref $stm->{else}[0]{pblock} &&
                ref $stm->{else}[0]{pblock}{block}) {

                $else = call 'postcircumfix:( )' => code($stm->{else}[0]{pblock}{block}),[capturize];
            }

            my @elsif;
            if (ref $stm->{elsif} eq 'ARRAY') {
                foreach my $elsif_part (@{$stm->{elsif}}) {

                    my $elsif = call 'postcircumfix:( )' => code($elsif_part->{xblock}{pblock}{block}),[capturize];

                    push @elsif, AST::If->new
                      ( cond => $elsif_part->{xblock}{EXPR}->emit_m0ld,
                        then => $elsif );
                }
            }

            AST::If->new
                ( cond => $stm->{xblock}{EXPR}->emit_m0ld,
                  then => $then,
                  else => $else,
                  elsif => \@elsif );

	} elsif ($stm->{sym} eq 'CONTROL') {
	    # CONTROL blocks are moved to the top of the
	    # statementlist, so we know that no code was executed
	    # before this, so we can peacefully delay the setup of the
	    # control block up to this point.
	    call 'set_control' => (call 'continuation' => reg '$interpreter'), [ code($stm->{block}) ];
	} elsif ($stm->{sym} eq 'CATCH') {
	    # the same for CATCH blocks.
	    call 'set_catch' => (call 'continuation' => reg '$interpreter'), [ code($stm->{block}) ];
        } else {
            XXX('unkown sym in statement_control')
        }
    } elsif ($m->{EXPR}) {
        $m->{EXPR}->emit_m0ld;
    } else {
        XXX('unknown statement')
    }
}


1;
