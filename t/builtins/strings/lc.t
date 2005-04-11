#!/usr/bin/pugs

use v6;
require Test;

plan 7;

is(lc("Hello World"), "hello world", "simple lc test");
is(lc(""), "", "empty string");
is(lc("ÅÄÖ"), "åäö", "some finnish non-ascii chars");
is(lc("ÓÒÚÙ"), "óòúù", "accented chars");

$_ = "Hello World"; 
my $x = lc;
is($x, "hello world", 'lc uses $_ as default');

{ # test invocant syntax for lc
    my $x = "Hello World";
    is($x.lc, "hello world", '$x.lc works');
    is("Hello World".lc, "hello world", '"Hello World".lc works');
}
