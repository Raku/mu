#!/usr/bin/pugs

require Test;
use v6;

plan 3;

# L<S06/"The C<caller> function">

# caller.sub
sub a_sub { b_sub() }
sub b_sub { eval 'caller.sub' }
todo_is ~a_sub(), "foo", "caller.sub works";

# caller.file
todo_ok index(~eval('caller.file'), "caller") >= 0, "caller.file works";

# caller.line (XXX: make sure to edit the expected line number!)
todo_is +eval('caller.line'), 19, "caller.line works";
