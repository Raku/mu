#!/usr/bin/perl6
use v6;

require Test;
require Email::Simple;

plan 1;

my Email::Simple $m .= new(source => "Foo-Bar: Baz\n\ntest\n");
$m.header("Foo-bar") = "quux";
is $m.as_string, "Foo-Bar: quux

test\n", "Only one header this time";
