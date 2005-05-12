#!/usr/bin/pugs

use v6;
use Test;

plan 4;

ok(eval 'my Int $foo; 1', 'compile my Int $foo');
ok(eval 'my Str $bar; 1', 'compile my Str $bar');

ok(eval 'my Int $foo; ref $foo == Int', 'Int $foo isa Int');
ok(eval 'my Str $bar; ref $bar == Str', 'Str $bar isa Str');
