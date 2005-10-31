#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::MM::Attribute');

foreach my $name ('$.foo', '$:foo', '@.foo', '@:foo', '%.foo', '%:foo', '&.foo', '&:foo') {
    my $a = attribute->new($name);
    isa_ok($a, 'attribute');
}
