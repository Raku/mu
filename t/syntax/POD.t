#!/usr/bin/pugs

use v6;
require Test;

plan 3;

# L<S02/"Literals">
# others will be added later, or u can do it.

=begin DATA
line1
line2
line3
=end DATA

todo_eval_is('+@=DATA', 3, 'the @=DATA return the correct size');
todo_eval_is('@=DATA[0]', 'line1', '@=DATA[0] is correct');
todo_eval_is('@-DATA[2]', 'line3', '@=DATA[2] is correct');