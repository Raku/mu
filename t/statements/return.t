#!/usr/bin/pugs

use v6;
require Test;

plan 6;

=pod

Basic tests for "return"

I noticed some odd behavior with the return 
statement, so I decided to write a test for it.

=cut

sub foo { return 1; }
is(foo(), 1, '... foo() returned 1 correctly');

eval 'sub bar { return }';
eval_is('bar()', undef, '... bare return statement returned undef');

sub bar2 { return() }
is(bar2(), undef, '... bare return statement w/ parens returned undef');

sub baz { return 10 if 1; }
is(baz(), 10, '... return worked with a statement modifier');

eval 'sub foobar { return if 1; }';
eval_is('foobar()', undef, '... bare return worked with a statement modifier');

sub foobar2 { return() if 1; }
is(foobar2(), undef, '... bare return worked with a statement modifier');