#!/usr/bin/pugs
use v6;

use Test;
plan 4;

use HTTP::Headers; pass "(dummy instead of broken use_ok)";

{
    my $h;
    
    ok($h = ::HTTP::Headers.new(), '1 - $h was initialized');
    ok($h.isa('HTTP::Headers'), '1 - $h.isa(HTTP::Headers)');
    is($h.as_string(), '', '1 - correct stringification');
}
