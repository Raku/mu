#!/usr/bin/pugs

use v6;
require Test;

plan 2;

=pod

I/O Redirection to scalar tests

=cut

todo_ok(eval { open $*OUT,">",\$scalar },'Direct STDOUT to a scalar');
todo_ok(eval { open $*ERR,">",\$scalar },'Direct STDERR to a scalar');