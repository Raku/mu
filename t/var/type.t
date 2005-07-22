#!/usr/bin/pugs

use v6;
use Test;

plan 4;

ok(try{my Int $foo; 1}, 'compile my Int $foo');
ok(try{my Str $bar; 1}, 'compile my Str $bar');

ok(try{my Int $foo; $foo ~~ Int}, 'Int $foo isa Int', :todo<feature>);
ok(try{my Str $bar; $bar ~~ Str}, 'Str $bar isa Str', :todo<feature>);
