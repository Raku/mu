#!/usr/bin/pugs

use v6;

require Test;
plan 8;

=head1 DESCRIPTION

Basic C<keys> and C<values> tests, see S29.

=cut

my @array = <a b c d>;
# L<S29/"Perl6::Arrays" /"keys"/>
is ~@array.keys,          '0 1 2 3', "keys on arrays";
# L<S29/"Perl6::Arrays" /"values"/>
eval_is '~@array.values', 'a b c d', "values on arrays";

my %hash = (a => 1, b => 2, c => 3, d => 4);
# L<S29/"Perl6::Hashes" /"keys"/>
is ~%hash.keys.sort,   "a b c d", "keys on hashes";
# L<S29/"Perl6::Hashes" /"values"/>
is ~%hash.values.sort, "1 2 3 4", "values on hashes";

my $pair = (a => 42);
is +$pair.keys,     1, "keys on pairs (1)";
is ~$pair.keys,   "a", "keys on pairs (2)";
is +$pair.values,   1, "values on pairs (1)";
is ~$pair.values,  42, "values on pairs (2)";
