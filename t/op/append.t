#!/usr/bin/pugs

use v6;
require Test;

plan(3);

# Again, mostly stolen from Perl 5

my $a = 'ab' ~ 'c';
my $b = 'def';

my $c = $a ~ $b;
ok($c eq 'abcdef', "~ two vars");

eval '$c ~= "xyz"';
todo_ok($c eq 'abcdefxyz', "~= a literal string");

my $_ = $a;
eval '$_ ~= $b';
todo_ok($_ eq 'abcdef',"~= var");
