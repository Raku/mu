#!/usr/bin/pugs

use v6;
require Test;

=kwid

for statement tests

=cut

plan 18;

## for with plain old range operator w/out parens

my $a;
eval 'for 0 .. 5 { $a = $a ~ $_; }';
is($a, '012345', 'for 0..5 {} works');

# ... with 'pointer'

my $b;
eval 'for 0 .. 5 -> { $b = $b ~ $_; }';
is($b, '012345', 'for 0 .. 5 -> {} works');

# ... with , sub

my $c;
eval 'for 0 .. 5, sub { $c = $c ~ $_; }';
todo_is($c, '012345', 'for 0 .. 5, sub {} works');

# ... with referential sub

my $d;
sub some_sub ($arg) { $d = $d ~ $arg; }
eval 'for 0 .. 5, &some_sub;';
todo_is($d, '012345', 'for 0 .. 5, &some_sub works');

## and now with parens around the range operator

my $e;
eval 'for (0 .. 5) { $e = $e ~ $_; }';
is($e, '012345', 'for () {} works');

# ... with 'pointer'

my $f;
eval 'for (0 .. 5) -> { $f = $f ~ $_; }';
is($f, '012345', 'for () -> {} works');

# ... with sub

my $g;
eval 'for (0 .. 5), sub { $g = $g ~ $_; }';
todo_is($g, '012345', 'for (0 .. 5), sub {} works');

# ... with referential sub

my $h;
sub some_sub_2 ($arg) { $h = $h ~ $arg; }
eval 'for (0 .. 5), &some_sub_2;';
todo_is($h, '012345', 'for (0 .. 5), &some_sub works');

## and now for with 'topical' variables

# ... w/out parens

my $i;
eval 'for 0 .. 5 -> $topic { $i = $i ~ $topic; }';
is($i, '012345', 'for 0 .. 5 -> $topic {} works');

# ... with parens

my $j;
eval 'for (0 .. 5) -> $topic { $j = $j ~ $topic; }';
is($j, '012345', 'for () -> $topic {} works');


## for with @list operator w/out parens

my @list_k = (0 .. 5);
my $k;
eval 'for @list_k { $k = $k ~ $_; }';
is($k, '012345', 'for @list {} works');

# ... with 'pointer'

my @list_l = (0 .. 5);
my $l;
eval 'for @list_l -> { $l = $l ~ $_; }';
is($l, '012345', 'for @list -> {} works');

# ... with , sub

my @list_m = (0 .. 5);
my $m;
eval 'for @list_m, sub { $m = $m ~ $_; }';
todo_is($m, '012345', 'for @list, sub {} works');

# ... with referential sub

my @list_n = (0 .. 5);
my $n;
sub some_sub ($arg) { $n = $n ~ $arg; }
eval 'for @list_n, &some_sub;';
todo_is($n, '012345', 'for @list, &some_sub works');

## and now with parens around the @list

my @list_o = (0 .. 5);
my $o;
eval 'for (@list_o) { $o = $o ~ $_; }';
is($o, '012345', 'for (@list) {} works');

# ... with 'pointer'

my @list_p = (0 .. 5);
my $p;
eval 'for (@list_p) -> { $p = $p ~ $_; }';
is($p, '012345', 'for (@list) -> {} works');

# ... with sub

my @list_q = (0 .. 5);
my $q;
eval 'for (@list_q), sub { $q = $q ~ $_; }';
todo_is($q, '012345', 'for (@list), sub {} works');

# ... with referential sub

my @list_r = (0 .. 5);
my $r;
sub some_sub_2 ($arg) { $r = $r ~ $arg; }
eval 'for (@list_r), &some_sub_2;';
todo_is($r, '012345', 'for (@list), &some_sub works');

