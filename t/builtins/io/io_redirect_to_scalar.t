#!/usr/bin/pugs

use v6;
use Test;

plan 2;

=pod

I/O Redirection to scalar tests

=cut

ok(eval { open $*OUT,">",\$scalar },'Direct STDOUT to a scalar', :todo(1));
ok(eval { open $*ERR,">",\$scalar },'Direct STDERR to a scalar', :todo(1));
