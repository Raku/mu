use v6-alpha;

use Test;

=pod

You can specify 'is copy' on an arg and modify it inside a function,
but this bug prevents it when the argument is optional *and* has a
default value.

(Note that "is copy = 0" is fine but "= 0 is copy" is not.)

=cut

plan 1;
sub boom ($arg is copy = 0) { $arg++ }

lives_ok { boom(42) }, "can modify a copy";


