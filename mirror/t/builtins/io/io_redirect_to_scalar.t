#!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

I/O Redirection to scalar tests

=cut

skip_rest("needs speccing"); exit;

# XXX: gaal: dunno how this should be, but this isn't it.
ok(eval { open $*OUT,">",\$scalar },'Direct STDOUT to a scalar', :todo);
ok(eval { open $*ERR,">",\$scalar },'Direct STDERR to a scalar', :todo);
