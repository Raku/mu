#!/usr/bin/pugs

use v6;
use Test;

BEGIN { unshift @*INC, '../../lib'; }

my @s_tests = (

   "foo"                     => "foo",
   "foo=bar"                 => "foo=bar",
   "   foo   "               => "foo",
   "foo="                    => 'foo=""',
   "foo=bar bar=baz"         => "foo=bar; bar=baz",
   "foo=bar;bar=baz"         => "foo=bar; bar=baz",
   'foo bar baz'             => "foo; bar; baz",
   'foo="\"" bar="\\\\"'     => 'foo="\""; bar="\\\\"',
   'foo,,,bar'               => 'foo, bar',
   'foo=bar,bar=baz'         => 'foo=bar, bar=baz',

   'text/html; charset=iso-8859-1' =>
    'text/html; charset=iso-8859-1',

   'foo="bar"; port="80,81"; discard, bar=baz' =>
    'foo=bar; port="80,81"; discard, bar=baz',

   'Basic realm="\"foo\\\\bar\""' =>
    'Basic; realm="\"foo\\\\bar\""',
);

my $extra_tests = 2;

plan(@s_tests + $extra_tests);

use_ok('HTTP::Headers::Util');

my $test_num = 0;

for @s_tests -> $test is copy {
    my $arg = $test[0];
    my $expect = $test[1];
    
    my $temp = split_header_words([$arg]);
    
    is join_header_words($temp), $expect, 'conversion test ' ~ (++$test_num);
}
