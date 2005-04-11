#!/usr/bin/pugs

use v6;
require Test;

plan 18;

=pod

Basic C<keys> and C<values> tests, see S29.

=cut

my @array = <a b c d>;

# L<S29/"Perl6::Arrays" /"keys"/>
is(~@array.keys, '0 1 2 3', '@arrays.keys works');
is(~keys(@array), '0 1 2 3', 'keys(@array) works');
is(+@array.keys, +@array, 'we have the same number of keys as elements in the array');

# L<S29/"Perl6::Arrays" /"values"/>
is(~@array.values, 'a b c d', '@array.values works');
is(~values(@array), 'a b c d', 'values(@array) works');
is(+@array.values, +@array, 'we have the same number of values as elements in the array');

my %hash = (a => 1, b => 2, c => 3, d => 4);

# L<S29/"Perl6::Hashes" /"keys"/>
is(~%hash.keys.sort, "a b c d", '%hash.keys works');
is(~sort(keys(%hash)), "a b c d", 'keys(%hash) on hashes');
is(+%hash.keys, +%hash, 'we have the same number of keys as elements in the hash');

# L<S29/"Perl6::Hashes" /"values"/>
is(~%hash.values.sort, "1 2 3 4", '%hash.values works');
is(~sort(values(%hash)), "1 2 3 4", 'values(%hash) works');
is(+%hash.values, +%hash, 'we have the same number of keys as elements in the hash');

# keys and values on Pairs
my $pair = (a => 42);
is(~$pair.keys,   "a", '$pair.keys works');
is(~keys($pair),   "a", 'keys($pair) works');
is(+$pair.keys,     1, 'we have one key');

is(~$pair.values,  42, '$pair.values works');
is(~values($pair),  42, 'values($pair) works');
is(+$pair.values,   1, 'we have one value');
