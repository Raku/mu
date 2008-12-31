package VAST::dotty;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my ($m,$noun) = @_;
    if (my $methodop = $m->{dottyop}{methodop}) {
        if ($methodop->{longname}) {
            my $positional = $methodop->{semilist}[0]{statement}[0];
            my @positional = $positional ? $positional->emit_m0ld : ();
            my $ident = $methodop->{longname}->canonical;
            if ($m->{sym} eq '.^!') {
                $ident = '^!' . $ident;
            }
            AST::Call->new(
                identifier=>string $ident,
                capture=>AST::Capture->new(invocant=>FETCH($noun),positional=>[@positional]),
            );
        } else {
            XXX('unknown methodop');
        }
    } elsif (my $postop = $m->{dottyop}{postop}) {
        if (my $postcircumfix = $postop->{postcircumfix}) {
            my $positional = $postcircumfix->{semilist}{statement}[0];
            my @positional = $positional ? $positional->emit_m0ld : ();
            if ($postcircumfix->{sym}[0] eq '(' && $postcircumfix->{sym}[1] eq ')') {
                call 'postcircumfix:( )' => FETCH($noun),[capturize([@positional])];
            } else {
                call ('postcircumfix:'.$postcircumfix->{sym}[0].' '.$postcircumfix->{sym}[1] => FETCH($noun),[@positional]);
            }
        } else {
            XXX('unknown postop');
        }
    } else {
        XXX('unknown dotty');
    }
}

1;
