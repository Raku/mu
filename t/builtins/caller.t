#!/usr/bin/pugs

use Test;
use v6;

plan 3;

# L<S06/"The C<caller> function">

# caller.subname
sub a_sub { b_sub() }
sub b_sub { eval 'caller.subname' }
is ~a_sub(), "foo", "caller.sub works", :todo;

# caller.file
ok index(~eval('caller.file'), "caller") >= 0, "caller.file works", :todo;

# caller.line (XXX: make sure to edit the expected line number!)
is +eval('caller.line'), 19, "caller.line works", :todo;
