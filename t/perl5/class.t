#!/usr/bin/pugs

use v6;
use Test;
use perl5:CGI;

plan(2);

unless try({ eval("1", :lang<perl5>) }) {
    skip_rest;
    exit;
}

{
    my $q;
    lives_ok {
    eval q| $q = CGI.new; |
        or die $!;
    }, "perl5:CLASS.new";

    is $q.isa(CGI), 1, "Correct isa";
}
