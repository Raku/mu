#!/usr/bin/pugs

use v6;

require Test;
plan 4;

=head1 DESCRIPTION

Basic C<keys> and C<values> tests, see S29.

=cut

my @array = <a b c d>;
# L<S29/"Perl6::Arrays" /"keys"/>
todo_eval_is '~@array.keys',   '0 1 2 3', "keys on arrays";
# L<S29/"Perl6::Arrays" /"values"/>
todo_eval_is '~@array.values', 'a b c d', "values on arrays";

my %hash = (a => 1, b => 2, c => 3, d => 4);
# L<S29/"Perl6::Hashes" /"keys"/>
todo_eval_is '~%hash.keys.sort:{ $^a cmp $^b }',   "a b c d", "keys on hashes";
# L<S29/"Perl6::Hashes" /"values"/>
todo_eval_is '~%hash.values.sort:{ $^a <=> $^b }', "1 2 3 4", "values on hashes";
