#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';

use Class::Inspector;

my $m; BEGIN { use_ok($m = "Blondie::Nodes") }

foreach my $class (@{ Class::Inspector->subclasses("Blondie::Node") }) {
    next unless $class =~ /^Blondie::(\w+)$/;
    my $type = $1;

    can_ok($m, $type);
    isa_ok($m->$type, $class);
}

can_ok("Blondie::Thunk", "digest");

