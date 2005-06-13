#!/usr/bin/pugs

use v6;
use Test;

=kwid

while statement tests

L<S04/"Loop statements">

=cut

plan 11;

my $i = 0;
while $i < 5 { $i++; };
is($i, 5, 'while $i < 5 {} works');

my $i = 0;
while 5 > $i { $i++; };
is($i, 5, 'while 5 > $i {} works');

# with parens

my $i = 0;
while ($i < 5) { $i++; };
is($i, 5, 'while ($i < 5) {} works');

my $i = 0;
while (5 > $i) { $i++; };
is($i, 5, 'while (5 > $i) {} works');

# single value
my $j = 0;
while 0 { $j++; };
is($j, 0, 'while 0 {...} works');

my $k = 0;
while $k { $k++; };
is($k, 0, 'while $var {...} works');


# other tests
{
        # this seems like a bit of a messy test, but the point is being able to
        # declare my $x within the while statement more suited for a file read
        # or iterator, but I didn't feel like creating one just for this test.
	eval_is(
		'my $y; while( (my $x = 2) == 2 ) { $y = $x; last; } $y',
		2,
		"'my' variable within 'while' conditional",
	:todo<bug>);
}

# while ... -> $x {...}
{
  my @array = (0..5);
  my $was_in_while;
  my @new;
  eval 'while @array.shift -> $x { $was_in_while++; push @new, $x }';
  ok $was_in_while,  'while ... -> $x {...} worked (1)':todo<bug>;
  is ~@new, ~@array, 'while ... -> $x {...} worked (1)':todo<bug>;
}

{
  my @array = (0..5);
  my $was_in_while;
  my @new;
  eval 'while shift @array -> $x { $was_in_while++; push @new, $x }';
  ok $was_in_while,  'while ... -> $x {...} worked (1)':todo<bug>;
  is ~@new, ~@array, 'while ... -> $x {...} worked (1)':todo<bug>;
}
