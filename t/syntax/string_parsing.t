#!/usr/bin/pugs
use v6;

# No C<require Test> here because we've to check method calls in strings, see
# below...

say "1..2";

# L<S02/"Literals" /"In order to interpolate the result of a method call">

my $a = -3;
if(eval '"$a.abs()"') {
  say "ok 1 - parsing method call inside a string";
} else {
  say "not ok 1 - parsing method call inside a string";
}

eval 'my $bar = "$*OUT.say(\'ok 2 - running a method call inside a string\')"';
