#!/usr/bin/pugs

use v6;
require Test;

plan 4;

todo_ok(eval 'my Int $foo; 1', 'compile my Int $foo');
todo_ok(eval 'my Str $bar; 1', 'compile my Str $bar');

todo_ok(eval 'my Int $foo; ref $foo == Int', 'Int $foo isa Int');
todo_ok(eval 'my Str $bar; ref $bar == Str', 'Str $bar isa Str');
