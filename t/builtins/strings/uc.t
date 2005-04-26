#!/usr/bin/pugs

use v6;
use Test;

plan 8;
force_todo 8;

is(uc("Hello World"), "HELLO WORLD", "simple");
is(uc(""), "", "empty string"); 
is(uc("åäö"), "ÅÄÖ", "some finnish non-ascii chars");
is(uc("óòúù"), "ÓÒÚÙ", "accented chars");

# given does not return proper value yet
$_ = "Hello World";
my $x = uc;
is $x, "HELLO WORLD", 'uc uses the default $_';

{   
    my $x = "Hello World";
    is $x.uc, "HELLO WORLD", '$x.uc works';
    is "Hello World".uc, "HELLO WORLD", '"Hello World".uc works';
}

# Bug: GERMAN SHARP S ("ß") should uc() to "SS", but it doesn't
# Compare with: perl -we 'use utf8; print uc "ß"'
is(uc("ß"), "SS", "uc() of non-ascii chars may result in two chars");
