#!/usr/bin/pugs
use v6;
require Test;

plan 1;

todo_fail("FIXME parsefail");
#ok(not eval 'my @a = (1); @a<0>')   # eval should return undef on syntax error
