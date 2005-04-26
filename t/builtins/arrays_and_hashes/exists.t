#!/usr/bin/pugs

use v6;

use Test;
plan 14;

=head1 DESCRIPTION

Basic C<exists> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"exists"/>
my @array = <a b c d>;
ok @array.exists(0),    "exists(positive index) on arrays (1)";
ok @array.exists(1),    "exists(positive index) on arrays (2)";
ok @array.exists(2),    "exists(positive index) on arrays (3)";
ok @array.exists(3),    "exists(positive index) on arrays (4)";
ok !@array.exists(4),   "exists(positive index) on arrays (5)";
ok !@array.exists(42),  "exists(positive index) on arrays (2)";
ok @array.exists(-1),   "exists(negative index) on arrays (1)";
ok @array.exists(-2),   "exists(negative index) on arrays (2)";
ok @array.exists(-3),   "exists(negative index) on arrays (3)";
ok @array.exists(-4),   "exists(negative index) on arrays (4)";
ok !@array.exists(-5),  "exists(negative index) on arrays (5)";
ok !@array.exists(-42), "exists(negative index) on arrays (6)";

# L<S29/"Perl6::Hashes" /"exists"/>
my %hash = (a => 1, b => 2, c => 3, d => 4);
ok %hash.exists("a"),   "exists on hashes (1)";
ok !%hash.exists("42"), "exists on hashes (2)";
