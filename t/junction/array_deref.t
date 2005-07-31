#!/usr/bin/pugs
use v6;
use Test;

plan 1;

my $junc = 0|1|2;
my @a = (0,1,2);
my $bool = bool::false;
try { $bool = (@a[$junc] == $junc) };
ok $bool, :todo<bug>;
