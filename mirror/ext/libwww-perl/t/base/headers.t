#!/usr/bin/pugs
use v6;

use Test;
plan 4;

use_ok('HTTP::Headers');

{
    my $h;
    
    ok($h = ::HTTP::Headers.new(), '1 - $h was initialized');
    ok($h.isa('HTTP::Headers'), '1 - $h.isa(HTTP::Headers)');
    is($h.as_string(), '', '1 - correct stringification');
}
