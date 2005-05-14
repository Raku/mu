#!/usr/bin/pugs

use v6;
use Test;

plan 5;

my @x = (1,2,3,4,5);
my $foo = [+] @x;
is($foo, 15,'Reduce [+]');
eval_ok('my $foo = [+] (1..5);','Parse [+]');

$foo = [*] @x;
is($foo, 120,'Reduce [*]');
eval_ok('my $foo = [*] (1..5);','Parse [*]');

eval_ok('my $foo = [<] (1..5);','Parse [<]', :todo<bug>);
eval_ok('my $foo = [>] (1..5);','Parse [>]', :todo<bug>);

eval_ok('my @foo = [1..3] >>+<< [1..3] >>+<< [1..3];','Sanity Check');
eval_ok('my @foo = [>>+<<] ([1..3],[1..3],[1..3]);','Parse [>>+<<]', :todo<bug>);
