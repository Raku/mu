#!/usr/bin/pugs

use v6;
use Test;

plan 8;

# Testing "is lazy" as proposed by luqui, hoping that it actually does make 
# it into the language.  We shall see, though.
# Message-ID: <7ca3f0160511152344q3e873eedn5c8a5ee026e7ae78[at]mail.gmail.com>

sub myif ($cond, $true is lazy, $false is lazy) {
    if $cond {
        $true();
    }
    else {
        $false();
    }
}

my $x;
is(myif(1, ($x = 1), ($x = 2)), 1, "true case");
is($x, 1, "the right one got executed");
is(myif(0, ($x = 1), ($x = 2)), 2, "false case");
is($x, 2, "the right one got executed");

sub dont ($code is lazy) {
    $code;   # this shouldn't do anything
    return;
}

$x = 42;
dont($x = 3);
is($x, 42, "didn't execute");
dont(code => ($x = 4));
is($x, 42, "didn't execute in named param");

sub please ($code is lazy) {
    $code();
}

please($x = 3);
is($x, 3, "did execute");
please(code => ($x = 4));
is($x, 4, "did execute in named param");

# vim: ft=perl6 :
