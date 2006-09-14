use v6-alpha;
use Test;

# L<S02/"Built-In Data Types"/"A variable's type is a constraint indicating what sorts">

plan 4;

ok(try{my Int $foo; 1}, 'compile my Int $foo');
ok(try{my Str $bar; 1}, 'compile my Str $bar');

ok(do{my Int $foo; $foo ~~ Int}, 'Int $foo isa Int');
ok(do{my Str $bar; $bar ~~ Str}, 'Str $bar isa Str');
