#!/usr/bin/pugs

use v6;
require Test;

plan 1;

=pod 

Basic tests for the log() and log10() builtins

=cut

# will this be the same on all machines? or should I truncate it?

todo_eval_is('log(5)', 1.6094379124341, 'got the log of 5');

# please write a test for log10()