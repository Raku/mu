#!/usr/bin/pugs

use v6;
require Test;

plan 3;

=pod 

Basic tests for the sign() builtin

=cut

todo_eval_is('sign(0)', 0, 'got the right sign for 0');
todo_eval_is('sign(-100)', -1, 'got the right sign for -100');
todo_eval_is('sign(100)', 1, 'got the right sign for 100');