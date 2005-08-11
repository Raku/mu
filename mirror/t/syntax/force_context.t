#!/usr/bin/pugs

use v6;
use Test;

=kwid

Context forcing operators

=cut

plan 32;

# string context

my $a = '2 is my favorite number';
is(ref(+$a), 'Num', 'it is forced into a Num');
is(+$a, 2, 'forced into numeric context');

my $b = 'Did you know that, 2 is my favorite number';
is(ref(+$b), 'Num', 'it is forced into a Num');
is(+$b, 0, 'non numbers forced into numeric context are 0');

# numeric context

my $c = 10.500000;
is(ref(~$c), 'Str', 'it is forced into a Str');
is(~$c, '10.5', 'forced into string context');

my $d = -100;
is(ref(~$d), 'Str', 'it is forced into a Str');
is(~$d, '-100', 'forced into string context');

my $e = -100.1010;
is(ref(~$e), 'Str', 'it is forced into a Str');
is(~$e, '-100.101', 'forced into string context');

# boolean context

my $f = '';
is(ref(?$f), 'Bool', 'it is forced into a Bool');
ok(!(?$f), 'it is forced into boolean context');

my $g = 'This will be true';
is(ref(?$g), 'Bool', 'it is forced into a Bool');
ok(?$g, 'it is forced into boolean context');

my $h = 0;
is(ref(?$h), 'Bool', 'it is forced into a Bool');
ok(!(?$h), 'it is forced into boolean context');

my $i = 1;
is(ref(?$i), 'Bool', 'it is forced into a Bool');
ok(?$i, 'it is forced into boolean context');

# ! boolean context

my $j = '';
is(ref(!$j), 'Bool', 'it is forced into a Bool');
ok(!$j, 'it is forced into boolean context');

my $k = 'This will be true';
is(ref(!$k), 'Bool', 'it is forced into a Bool');
ok(!(!$k), 'it is forced into boolean context');

my $l = 0;
is(ref(!$l), 'Bool', 'it is forced into a Bool');
ok(!$l, 'it is forced into boolean context');

my $m = 1;
is(ref(!$m), 'Bool', 'it is forced into a Bool');
ok(!(!$m), 'it is forced into boolean context');

# int context

my $n = '2 is my favorite number';
is(ref(int($n)), 'Int', 'it is forced into a Int');
is(+$n, 2, 'forced into integer context');

my $o = 'Did you know that, 2 is my favorite number';
is(ref(int($o)), 'Int', 'it is forced into a Int');
is(int($o), 0, 'non numbers forced into integer context are 0');

my $p = 1.21122111;
is(ref(int($p)), 'Int', 'it is forced into a Int');
is(int($p), 1, 'float numbers forced into integer context are 0');
