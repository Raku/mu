#!/usr/bin/pugs

use v6;

say "1..3";

try { die "foo" };
if $! eq "foo" { say "ok 1" } else { say "not ok 1" }

try { "this_does_not_die" };
unless $!      { say "ok 2" } else { say "not ok 2" }

try { die "bar" };
if $! eq "bar" { say "ok 3" } else { say "not ok 3" }
