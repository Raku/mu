#!/usr/bin/pugs

use v6;

require Test;

plan 3;

# L<S02/"Names" /Which line am I in/>
is($?LINE, 10, '$?LINE works');
# L<S02/"Names" /Which file am I in/>
is($?FILE, 't/magicals/file_line.t', '$?FILE works');

todo_fail("this test needs to use File::Spec");
