#!/usr/bin/pugs
use v6;
require Test;
use Sample::Module;

=kwid

Test Sample::Module

=cut

plan 1;

is(greeting('pugs'), 'hello, pugs');
