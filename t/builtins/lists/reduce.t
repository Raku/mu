#!/usr/bin/pugs

use Test;
use v6;

plan 6;

=head1 DESCRIPTION

This test tests the C<reduce> builtin.

Reference:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org>

=cut

my @array = <5 -3 7 0 1 -9>;
my $sum   = 5 + -3 + 7 + 0 + 1 + -9; # laziness :)

eval_is 'reduce:{ $^a + $^b } 0, @array', $sum,
  "basic reduce works (1)", :todo;
eval_is 'reduce:{ $^a + $^b } 100, @array', 100 + $sum,
  "basic reduce works (2)", :todo;


# New [...] metaoperator
# Thread "reduce metaoperator" from p6l
is ([+] @array),        $sum, "[+] works";
eval_is '[*] 1,2,3',   1*2*3, "[*] works";
eval_is '[-] 1,2,3',   1-2-3, "[-] works";
eval_is '[/] 12,4,3',      1, "[/] works";
