#!/usr/bin/pugs

use v6;
use Test;

plan(3);

unless eval 'eval_perl5("1")' {
    skip_rest;
    exit;
}

{
    my $r = eval_perl5("0");
    is($r, 0, "number");
}

{
    my $r = eval_perl5("2");
    is($r, 2, "number");
}

{
    my $r = eval_perl5("perl6 now");
    is($r, 'perl6 now', "string");
}

