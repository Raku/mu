#!/usr/bin/perl

require Test;

plan 4;

my @a = (1, 2, 3);
@a .= map {$_+1};
is (@a[0], 2, 'inplace map [0]');
is (@a[1], 3, 'inplace map [1]');
is (@a[2], 4, 'inplace map [2]');

my $a=3.14;

eval '$a .= int;';
todo_is ($a, 3, "inplace int");

