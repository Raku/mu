#!/usr/bin/pugs

use v6;
require Test;

plan(6);

my @unshift;
eval 'unshift @foo, 42';
todo_ok (@unshift[0] == 42,"unshift");
eval '@foo.unshift(24)';
todo_ok (@unshift[1] == 42,"unshift");
todo_ok (@unshift[0] == 24,"unshift");
eval 'unshift @foo, 1, 2, 3';
todo_ok (@unshift[0] == 1,"unshift");
todo_ok (@unshift[1] == 2,"unshift");
todo_ok (@unshift[2] == 3,"unshift");
