#!/usr/bin/pugs

use v6;
require Test;

plan(6);

my @pop = (1, 2, 3, 4, 5);

is(pop(@pop), 5, "pop 5");
todo_is(pop(@pop), 4, "pop 4");
todo_is(pop(@pop), 3, "pop 3");
todo_is(pop(@pop), 2, "pop 2");
todo_is(pop(@pop), 1, "pop 1");
is(pop(@pop), undef, "last pop")
