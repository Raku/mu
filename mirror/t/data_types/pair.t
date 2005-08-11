#!/usr/bin/pugs

use v6;
use Test;

=kwid 

 Pair test

=cut

plan 58;

# basic Pair

my $pair = 'foo' => 'bar';
isa_ok($pair, 'Pair');
is($pair.perl, "('foo' => 'bar')", 'canonical representation');

# get key and value from the pair as many ways as possible

is(key($pair), 'foo', 'got the right key($pair)');
is(value($pair), 'bar', 'got the right value($pair)');

is(key $pair, 'foo', 'got the right key $pair');
is(value $pair, 'bar', 'got the right value $pair');

is($pair.key(), 'foo', 'got the right $pair.key()');
is($pair.value(), 'bar', 'got the right $pair.value()');

is($pair.key, 'foo', 'got the right $pair.key');
is($pair.value, 'bar', 'got the right $pair.value');

# get both (kv) as many ways as possible

my @pair1a = kv($pair);
is(+@pair1a, 2, 'got the right number of elements in the list');
is(@pair1a[0], 'foo', 'got the right key');
is(@pair1a[1], 'bar', 'got the right value');

my @pair1b = kv $pair;
is(+@pair1b, 2, 'got the right number of elements in the list');
is(@pair1b[0], 'foo', 'got the right key');
is(@pair1b[1], 'bar', 'got the right value');

my @pair1c = $pair.kv;
is(+@pair1c, 2, 'got the right number of elements in the list');
is(@pair1c[0], 'foo', 'got the right key');
is(@pair1c[1], 'bar', 'got the right value');

my @pair1d = $pair.kv();
is(+@pair1d, 2, 'got the right number of elements in the list');
is(@pair1d[0], 'foo', 'got the right key');
is(@pair1d[1], 'bar', 'got the right value');

# Pair with a numeric value

my $pair2 = 'foo' => 2;
isa_ok($pair2, 'Pair');

is($pair2.value, 2, 'got the right value');

# Pair with a Pair value

my $pair3 = "foo" => ("bar" => "baz");
isa_ok($pair3, 'Pair');

my $pair3a = $pair3.value;
isa_ok($pair3a, 'Pair');
is($pair3a.key, 'bar', 'got right nested pair key');
is($pair3a.value, 'baz', 'got right nested pair key');

is($pair3.value.key, 'bar', 'got right nested pair key (method chaining)');
is($pair3.value.value, 'baz', 'got right nested pair key (method chaining)');

# Pair with a Pair key

my $pair4 = ("foo" => "bar") => "baz";
isa_ok($pair4, 'Pair');

is($pair4.value, 'baz', 'got the right value');

isa_ok($pair4.key, 'Pair');
is($pair4.key.key, 'foo', 'got right nested key');
is($pair4.key.value, 'bar', 'got right nested value');

my $quux = (quux => "xyzzy");
is($quux.key, 'quux', "lhs quotes" );

# lvalue Pair assignments from S06 and thread starting with
# http://www.nntp.perl.org/group/perl.perl6.language/19425

my $val;
("foo" => $val) = "baz";
is($val, "baz", "lvalue pairs");

# illustrate a bug

my $var   = 'foo' => 'bar';
sub test1 (Any|Pair $pair) {
	isa_ok($pair,'Pair');
	my $testpair = $pair;
	isa_ok($testpair,'Pair'); # new lvalue variable is also a Pair
	my $boundpair := $pair;
	isa_ok($boundpair,'Pair'); # bound variable is also a Pair
	is($pair.key, 'foo', 'in sub test1 got the right $pair.key');
	is($pair.value, 'bar', 'in sub test1 got the right $pair.value');

}
test1 $var;

my %hash  = ('foo' => 'bar');
for  %hash.pairs -> $pair {
	isa_ok($pair,'Pair',:todo<bug>) ; 
	my $testpair = $pair;
	isa_ok($testpair,'Pair',:todo<bug>); # new lvalue variable is also a Pair
	my $boundpair := $pair;
	isa_ok($boundpair,'Pair',:todo<bug>); # bound variable is also a Pair
	is($pair.key, 'foo', 'in for loop got the right $pair.key');
	is($pair.value, 'bar', 'in for loop got the right $pair.value');
}

sub test2 (Hash %h){
	for %h.pairs -> $pair {
		isa_ok($pair,'Pair',:todo<bug>) ; 
		is($pair.key, 'foo', 'in sub test2 got the right $pair.key');
		is($pair.value, 'bar', 'in sub test2 got the right $pair.value');
	}
}
test2 %hash;

# See thread "$pair[0]" on p6l started by Ingo Blechschmidt:
# http://www.nntp.perl.org/group/perl.perl6.language/22593

sub test3 (Hash %h){
	for %h.pairs -> $pair {
		isa_ok($pair,'Pair',:todo<bug>) ; 
		dies_ok({$pair[0]}, 'sub test3: access by $pair[0] should not work');
		dies_ok({$pair[1]}, 'sub test3: access by $pair[1] should not work');
	}
}
test3 %hash;

=begin p6l

Hm, Hash::pair? Never heard of that.  --iblech

sub test4 (Hash %h){
	for %h.pair -> $pair {
		isa_ok($pair,'Pair',:todo<bug>) ; 
		is($pair.key, 'foo', 'sub test4: access by unspecced "pair" got the right $pair.key');
		is($pair.value, 'bar', 'sub test4: access by unspecced "pair" got the right $pair.value');

	}
}
test4 %hash;

=end p6l

=cut

my $should_be_a_pair = (a => 25/1);
isa_ok $should_be_a_pair, "Pair", "=> has correct precedence";

=pod

Stated by Larry on p6l in:
http://www.nntp.perl.org/group/perl.perl6.language/20122

 "Oh, and we recently moved => to assignment precedence so it would
 more naturally be right associative, and to keep the non-chaining
 binaries consistently non-associative.  Also lets you say:

   key => $x ?? $y :: $z;

 plus it moves it closer to the comma that it used to be in Perl 5."

(iblech) XXX: this contradicts current S03 so I could be wrong.

=cut

{
  # This should always work.
  my %x = ( "Zaphod" => (0 ?? 1 :: 2), "Ford" => 42 );
  is %x{"Zaphod"}, 2, "Zaphod is 2";
  is %x{"Ford"},  42, "Ford is 42";

  # This should work only if => is lower precedence than ?? ::
  my %z = ( "Zaphod" => 0 ?? 1 :: 2, "Ford" => 42 );
  is %z{"Zaphod"}, 2, "Zaphod is still 2";
  is %z{"Ford"},  42, "Ford is still 42";
}
