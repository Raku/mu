#!/usr/bin/pugs

use v6;
require Test;

plan 1;

=pod 

Basic tests for the exp() builtin

=cut

# will this be the same on all machines? or should I truncate it?

todo_eval_is('exp(5)', 148.413159102577, 'got the exponent of 5');