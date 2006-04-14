#!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

I/O Redirection to scalar tests

=cut

skip_rest("needs speccing"); exit;

# XXX: gaal: dunno how this should be, but this isn't it.
ok(try { open $*OUT,">",\$scalar },'Direct STDOUT to a scalar', :todo);
ok(try { open $*ERR,">",\$scalar },'Direct STDERR to a scalar', :todo);
