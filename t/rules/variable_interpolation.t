#!/usr/bin/pugs

use v6;
use Test;

plan 3;

my $rule = '\d+';
ok('2342' ~~ rx:perl5{$rule}, 'interpolated rule applied successfully');

my $rule2 = 'he(l)+o';
ok('hello' ~~ rx:perl5{$rule2}, 'interpolated rule applied successfully');

my $rule3 = 'r+';
my $subst = 'z';
my $bar = "barrrr"; 
$bar ~~ s:perl5:g{$rule3}{$subst}; 
is($bar, "baz", 'variable interpolation in substitute regexp works with :g modifier');
