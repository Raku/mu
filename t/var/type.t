#!/usr/bin/pugs

use v6;
require Test;

plan 2;

todo_ok(eval 'my Int $foo; 1', 'compile my Int $foo');
todo_ok(eval 'my Str $bar; 1', 'compile my Str $bar');

