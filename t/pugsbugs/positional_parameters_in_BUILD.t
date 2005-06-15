#!/usr/bin/pugs

use v6;
use Test;
plan 1;

my $value_from_BUILD;

class Foo { submethod BUILD ($value) { $value_from_BUILD = $value; } };

my $foo = Foo.new("passed");

is $value_from_BUILD, 'passed', 'positional args passed to new() are passed on to BUILD', :todo<bug>;
