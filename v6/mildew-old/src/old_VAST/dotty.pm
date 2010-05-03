package VAST::dotty;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my ($m,$noun) = @_;
    if (my $methodop = $m->{dottyop}{methodop}) {
        if ($methodop->{longname}) {
            my @args = $methodop->{args}[0] ? $methodop->{args}[0]->emit_m0ld : ();
            my @positional = grep { ref $_ ne 'AST::Pair' } @args;
            my @named = map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @args;
            my $ident = $methodop->{longname}->canonical;
            if ($m->{sym} eq '.^!') {
                $ident = '^!' . $ident;
                AST::Call->new(
                    identifier=>string $ident,
                    capture=>AST::Capture->new(invocant=>FETCH($noun),positional=>[@positional],named=>[@named]),
                    );
            } elsif ($m->{sym} eq '.^') {
                let FETCH($noun), sub {
                    my $object = shift;
                    return call($ident => FETCH(call '^!how' => $object), [$object, @positional], [@named]);
                }
            } elsif ($m->{sym} eq '.') {
                AST::Call->new(
                    identifier=>string $ident,
                    capture=>AST::Capture->new(invocant=>FETCH($noun),positional=>[@positional],named=>[@named]),
                    );
            } else {
                XXX('unknown dotty sym');
            }
        } else {
            XXX('unknown methodop');
        }
    } elsif (my $postop = $m->{dottyop}{postop}) {
        if (my $postcircumfix = $postop->{postcircumfix}) {
            my @args;
            if (my $positional = $postcircumfix->{semilist}{statement}[0]) {
                @args = $positional->emit_m0ld;
            } elsif ($postcircumfix->{semiarglist}) {
                @args = $postcircumfix->{semiarglist}->emit_m0ld;
            }
            my @positional = grep { ref $_ ne 'AST::Pair' } @args;
            my @named = map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @args;
            if ($postcircumfix->{sym}[0] eq '(' && $postcircumfix->{sym}[1] eq ')') {
                call 'postcircumfix:( )' => FETCH($noun),[capturize([@positional],[@named])];
            } else {
                call ('postcircumfix:'.$postcircumfix->{sym}[0].' '.$postcircumfix->{sym}[1] => FETCH($noun),[@positional],[@named]);
            }
        } else {
            XXX('unknown postop');
        }
    } else {
        XXX('unknown dotty');
    }
}

1;
