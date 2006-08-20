use v6-alpha;
use Test;

plan 2;

=for Explanation

Pugs as of at least r12504 is segfaulting (at least on darwin-ppc)
when certain list operations are performed on hashes.

=cut

my %hash = %(zip('a'..'d';1..4));
my $i = %hash.elems; # segfaults
pass 'C<%hash.elems> does not segfault';

$i++ for %hash; # segfaults
pass 'C<$i++ for %hash> does not segfault';
