#!/usr/bin/pugs

use v6;
use Test;

=pod

You can specify 'is copy' on an arg and modify it inside a function,
but this bug prevents it when the argument is optional *and* has a
default value.

=cut

plan 1;
sub boom (?$arg = 0 is copy) { $arg++ }

lives_ok { boom(42) }, "can modify a copy", :todo<bug>;


