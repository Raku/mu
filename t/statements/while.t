#!/usr/bin/pugs

use v6;
require Test;

=kwid

while statement tests

L<S04/"Loop statements">

=cut

plan 10;

my $i = 0;
eval 'while $i < 5 { $i++; }';
is($i, 5, 'while $i < 5 {} works');

my $i = 0;
eval 'while 5 > $i { $i++; }';
is($i, 5, 'while 5 > $i {} works');

# with parens

my $i = 0;
eval 'while ($i < 5) { $i++; }';
is($i, 5, 'while ($i < 5) {} works');

my $i = 0;
eval 'while (5 > $i) { $i++; }';
is($i, 5, 'while (5 > $i) {} works');

# single value
my $j = 0;
eval 'while 0 { $j++; }';
is($j, 0, 'while 0 {...} works');

my $k = 0;
eval 'while $k { $k++; }';
is($k, 0, 'while $var {...} works');

# sub as comparator
my $l = 0;
eval 'while undef { $l++ }';
is($l, 0, 'while undef {...} works');

# next
my $actual = 0;
my $wanted = 3;
my $saw_next = 0;
while $wanted-- {
	#next if $wanted == 2;
	if $wanted == 2 { eval 'next' } # unevalme

	$actual++;
	#NEXT {
	#	$saw_next++;
	#}
}
todo_fail("FIXME_parsefail (NEXT block)");
todo_is($actual, 2, "next skips to next iteration");
todo_is($saw_next, 1, "NEXT block was called");
