#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests the "for" statement

This attemps to test as many variations of the
for statement as possible

L<S04/"The C<for> statement">

=cut

plan 34;

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
for (0 .. 5), sub { $c = $c ~ $_; };
is($c, '012345', 'for 0 .. 5, sub {} works');

# ... with referential sub

my $d;
sub some_sub_1 ($arg) { $d = $d ~ $arg; }
for (0 .. 5), &some_sub_1;
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
for (0 .. 5), sub { $g = $g ~ $_; };
is($g, '012345', 'for (0 .. 5), sub {} works');

# ... with referential sub

my $h;
sub some_sub_2 ($arg) { $h = $h ~ $arg; }
for (0 .. 5), &some_sub_2;
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


## for with @array operator w/out parens

my @array_k = (0 .. 5);
my $k;
for @array_k { $k = $k ~ $_; };
is($k, '012345', 'for @array {} works');

# ... with 'pointer'

my @array_l = (0 .. 5);
my $l;
for @array_l -> { $l = $l ~ $_; };
is($l, '012345', 'for @array -> {} works');

# ... with , sub

my @array_m = (0 .. 5);
my $m;
for (@array_m), sub { $m = $m ~ $_; };
is($m, '012345', 'for @array, sub {} works');

# ... with referential sub

my @array_n = (0 .. 5);
my $n;
sub some_sub_3 ($arg) { $n = $n ~ $arg; }
for (@array_n), &some_sub_3;
is($n, '012345', 'for @array, &some_sub works');

## and now with parens around the @array

my @array_o = (0 .. 5);
my $o;
for (@array_o) { $o = $o ~ $_; };
is($o, '012345', 'for (@array) {} works');

# ... with 'pointer'

my @array_p = (0 .. 5);
my $p;
for (@array_p) -> { $p = $p ~ $_; };
is($p, '012345', 'for (@array) -> {} works');

# ... with sub

my @array_q = (0 .. 5);
my $q;
for (@array_q), sub { $q ~= $_; };
is($q, '012345', 'for (@array), sub {} works');

# ... with referential sub

my @array_r = (0 .. 5);
my $r;
sub some_sub_4 ($arg) { $r ~= $arg; }
for (@array_r), &some_sub_4;
is($r, '012345', 'for (@array), &some_sub works');


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
    for (@elems) { push @a, $_, $_; }
    my @e = <a a b b c c d d e e>;
    is(@a, @e, 'for (@a) { ... $_ ... $_ ... } iterates all elems, not just odd');
}


# for with "is rw"

my @array_s = (0..2);
my @s = (1..3);
for @array_s -> { $_++ };
is(@array_s, @s, 'for @array { $_++ }');

my @array_t = (0..2);
my @t = (1..3);
for @array_t -> $val is rw { $val++ };
is(@array_t, @t, 'for @array -> $val is rw { $val++ }');

my @array_v = (0..2);
my @v = (1..3);
try { for @array_v.values -> $val is rw { $val++ }; };
is(@array_v, @v, 'for @array.values -> $val is rw { $val++ }', :todo<feature>);

my @array_kv = (0..2);
my @kv = (1..3);
try { for @array_kv.kv -> $key, $val is rw { $val++ }; };
is(@array_kv, @kv, 'for @array.kv -> $key, $val is rw { $val++ }', :todo<feature>);

my %hash_v = ( a => 1, b => 2, c => 3 );
my %v = ( a => 2, b => 3, c => 4 );
try { for %hash_v.values -> $val is rw { $val++ }; };
is(%hash_v, %v, 'for %hash.values -> $val is rw { $val++ }', :todo<feature>);

my %hash_kv = ( a => 1, b => 2, c => 3 );
my %kv = ( a => 2, b => 3, c => 4 );
try { for %hash_kv.kv -> $key, $val is rw { $val++ }; };
is( %hash_kv.sort, %kv.sort, 'for %hash.kv -> $key, $val is rw { $val++ }', :todo<feature>);


# .key //= ++$i for @array1;
{
   class TestClass is rw { has $.key; };
   my @array1 = (TestClass.new(),TestClass.new(:key<2>));
   my @array2 = (TestClass.new(:key<1>),TestClass.new(:key<3>));   
   
   my $i = 0;
   try { .key //= ++$i for @array1 };
   my $sum1 = @array1.map:{ $_.key };
   my $sum2 = @array2.map:{ $_.key };
   is( $sum1, $sum2, '.key //= ++$i for @array1;', :todo<bug>);

}

# .key = 1 for @array1;
{
   class TestClass is rw { has $.key; };
   my @array1 = (TestClass.new(),TestClass.new(:key<2>));
   my @array2 = (TestClass.new(:key<1>),TestClass.new(:key<1>));   

   try { .key = 1 for @array1 };
   my $sum1 = @array1.map:{ $_.key };
   my $sum2 = @array2.map:{ $_.key };
   is( $sum1, $sum2, '.key = 1 for @array1;', :todo<bug>);
}

# $_.key = 1 for @array1;
{
   class TestClass is rw { has $.key; };
   my @array1 = (TestClass.new(),TestClass.new(:key<2>));
   my @array2 = (TestClass.new(:key<1>),TestClass.new(:key<1>));   

   try { $_.key = 1 for @array1 };
   my $sum1 = @array1.map:{ $_.key };
   my $sum2 = @array2.map:{ $_.key };
   is( $sum1, $sum2, '$_.key = 1 for @array1;');

}