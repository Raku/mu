#!/usr/bin/pugs
use v6;
require Test;
require Sample::Module-0.0.1;

=kwid

Test Sample::Module

=cut

plan 1;

is(greeting('pugs'), 'hello, pugs');
