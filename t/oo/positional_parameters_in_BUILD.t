#!/usr/bin/pugs

use v6;
use Test;
plan 1;

my $value_from_BUILD;

class Foo { submethod BUILD ($value) { $value_from_BUILD = $value; } };

dies_ok { Foo.new("passed") }, 'positional args passed to new() should not be passed on to BUILD';
