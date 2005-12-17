#!/usr/bin/pugs

use v6;
use Test;

plan(3);

unless try { eval("1", :lang<perl5>) } {
    skip_rest;
    exit;
}

{
    my $r = eval("0", :lang<perl5>);
    is($r, 0, "number");
}

{
    my $r = eval("2", :lang<perl5>);
    is($r, 2, "number");
}

{
    my $r = eval('"perl6 now"', :lang<perl5>);
    is($r, 'perl6 now', "string");
}

