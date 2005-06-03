#!/usr/bin/pugs
use v6;

############################################################
# Coroutine solution by geoffb                             #
# + optional functionality in Algorithm::Loops NestedLoops #
############################################################

my @loops = ([1..3], ['a'..'e'], ['foo', 'bar']);

sub NestedLoop (++@loop, +$only_when, +$code) {
    my &iter = NL2(loop => @loop);

    sub {
        my @next = iter;
        return @next unless defined @next[0];

        if $only_when {
            return &?SUB() unless $only_when(@next);
        }

        $code(@next) if $code;
        return @next;
    }
}

sub NL2 (++@loop) {
    coro {
        given (@loop.elems) {
            when 0  { yield [] }
            when 1  { for @loop[0] { yield [$^first] } yield undef while 1 }
            default {
                for @loop[0] -> $first {
                    my &rest = NL2(loop => @loop[1..Inf]);
                    my @rest;
                    while @rest = rest() {
                       yield [$first, @rest];
                    }
                }
                yield undef while 1;
            }
        }
    }
}


my ($cnt, $item);

my &iter = NestedLoop(loop      => @loops,
                      only_when => sub { ++$cnt % 2 },
                      code      => sub {say "reversed: {reverse @^group}"});

say "ITER {$cnt}: {$item.perl}" while $item = iter;
