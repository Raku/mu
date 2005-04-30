#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests the "for" statement

This attemps to test as many variations of the
for statement as possible

L<S04/"The C<for> statement">

=cut

plan 27;
force_todo 3, 4, 7, 8, 17, 18, 22;

## for with plain old range operator w/out parens
# L<S04/"The C<for> statement" /in Perl 6, si it always take a list as an argument/>

my $a;
for 0 .. 5 { $a = $a ~ $_; };
is($a, '012345', 'for 0..5 {} works');

# ... with 'pointer'
# L<S04/"The C<for> statement" /to the closure:/>

my $b;
for 0 .. 5 -> { $b = $b ~ $_; };
is($b, '012345', 'for 0 .. 5 -> {} works');

# ... with , sub

my $c;
eval 'for 0 .. 5, sub { $c = $c ~ $_; }';
is($c, '012345', 'for 0 .. 5, sub {} works');

# ... with referential sub

my $d;
sub some_sub ($arg) { $d = $d ~ $arg; }
eval 'for 0 .. 5, &some_sub;';
is($d, '012345', 'for 0 .. 5, &some_sub works');

## and now with parens around the range operator

my $e;
for (0 .. 5) { $e = $e ~ $_; };
is($e, '012345', 'for () {} works');

# ... with 'pointer'

my $f;
for (0 .. 5) -> { $f = $f ~ $_; };
is($f, '012345', 'for () -> {} works');

# ... with sub

my $g;
eval 'for (0 .. 5), sub { $g = $g ~ $_; }';
is($g, '012345', 'for (0 .. 5), sub {} works');

# ... with referential sub

my $h;
sub some_sub_2 ($arg) { $h = $h ~ $arg; }
eval 'for (0 .. 5), &some_sub_2;';
is($h, '012345', 'for (0 .. 5), &some_sub works');

# ... with implicit topic

$_ = "GLOBAL VALUE";
for "INNER VALUE" {
  is( lc(), "inner value", "Implicit default topic is seen by lc()" );
};
is($_,"GLOBAL VALUE","After the loop the implicit topic gets restored");

$_ = "GLOBAL VALUE";
is( lc(), "inner value", "Implicit default topic is seen by lc()" )
  for "INNER VALUE";
is($_,"GLOBAL VALUE","After the loop the implicit topic gets restored");

## and now for with 'topical' variables

# ... w/out parens

my $i;
for 0 .. 5 -> $topic { $i = $i ~ $topic; };
is($i, '012345', 'for 0 .. 5 -> $topic {} works');

# ... with parens

my $j;
for (0 .. 5) -> $topic { $j = $j ~ $topic; };
is($j, '012345', 'for () -> $topic {} works');


## for with @list operator w/out parens

my @list_k = (0 .. 5);
my $k;
for @list_k { $k = $k ~ $_; };
is($k, '012345', 'for @list {} works');

# ... with 'pointer'

my @list_l = (0 .. 5);
my $l;
for @list_l -> { $l = $l ~ $_; };
is($l, '012345', 'for @list -> {} works');

# ... with , sub

my @list_m = (0 .. 5);
my $m;
eval 'for @list_m, sub { $m = $m ~ $_; }';
is($m, '012345', 'for @list, sub {} works');

# ... with referential sub

my @list_n = (0 .. 5);
my $n;
sub some_sub ($arg) { $n = $n ~ $arg; }
eval 'for @list_n, &some_sub;';
is($n, '012345', 'for @list, &some_sub works');

## and now with parens around the @list

my @list_o = (0 .. 5);
my $o;
for (@list_o) { $o = $o ~ $_; };
is($o, '012345', 'for (@list) {} works');

# ... with 'pointer'

my @list_p = (0 .. 5);
my $p;
for (@list_p) -> { $p = $p ~ $_; };
is($p, '012345', 'for (@list) -> {} works');

# ... with sub

my @list_q = (0 .. 5);
my $q;
eval 'for (@list_q), sub { $q = $q ~ $_; }';
is($q, '012345', 'for (@list), sub {} works');

# ... with referential sub

my @list_r = (0 .. 5);
my $r;
sub some_sub_2 ($arg) { $r = $r ~ $arg; }
eval 'for (@list_r), &some_sub_2;';
is($r, '012345', 'for (@list), &some_sub works');


my @elems = <a b c d e>;

{
	my @a;
	for (@elems) {
		push @a, $_;
	}
	my @e = <a b c d e>;
	is(@a, @e, 'for (@a) { ... $_ ... } iterates all elems');
}

{
	my @a;
    for (@elems) -> $_ { push @a, $_ };
	my @e = @elems;
	is(@a, @e, 'for (@a)->$_ { ... $_ ... } iterates all elems' );
}

{
	my @a;
	for (@elems) {
		push @a, $_, $_;
	}
	my @e = <a a b b c c d d e e>;
	is(@a, @e, 'for (@a) { ... $_ ... $_ ... } iterates all elems, not just odd');
}

my @list_s = (0..2);
my @s = (1..3);
eval 'for @list_s { $_++ }';
is(@list_s, @s, 'for @list { $_++ }');

my @list_t = (0..2);
my @t = (1..3);
eval 'for @list_t -> $num is rw { $num++ }';
is(@list_t, @t, 'for @list -> $num is rw { $num++ }', :todo);

