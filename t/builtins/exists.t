#!/usr/bin/pugs

use v6;

require Test;
plan 4;

=head1 DESCRIPTION

Basic C<exists> tests, see S29.

=cut

# L<S29/"Perl6::Arrays" /"exists"/>
my @array = <a b c d>;
todo_eval_ok '@array.exists(0)',   "exists on arrays (1)";
todo_eval_ok '!@array.exists(42)', "exists on arrays (2)";

# L<S29/"Perl6::Hashes" /"exists"/>
my %hash = (a => 1, b => 2, c => 3, d => 4);
todo_eval_ok '%hash.exists("a")',   "exists on hashes (1)";
todo_eval_ok '!%hash.exists("42")', "exists on hashes (2)";
