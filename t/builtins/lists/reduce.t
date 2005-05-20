#!/usr/bin/pugs

use Test;
use v6;

=head1 DESCRIPTION

This test tests the C<reduce> builtin and the reduce metaoperator C<[...]>.

References:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org> and
L<http://groups.google.de/group/perl.perl6.language/msg/bd9eb275d5da2eda>

=cut

plan 25;

{
  my @array = <5 -3 7 0 1 -9>;
  my $sum   = 5 + -3 + 7 + 0 + 1 + -9; # laziness :)

  is((reduce { $^a + $^b } 0, @array), $sum, "basic reduce works (1)");
  is((reduce { $^a + $^b } 100, @array), 100 + $sum, "basic reduce works (2)");

  is(([+] *@array),      $sum, "[+] works");
  is(([*]  1,2,3),    (1*2*3), "[*] works");
  is(([-]  1,2,3),    (1-2-3), "[-] works");
  is(([/]  12,4,3),  (12/4/3), "[/] works");
  is(([**] 2,2,3),  (2**2**3), "[**] works");
}

ok (    [<]  1, 2, 3, 4), "[<] works (1)";
ok (not [<]  1, 3, 2, 4), "[<] works (2)";
ok (    [>]  4, 3, 2, 1), "[>] works (1)";
ok (not [>]  4, 2, 3, 1), "[>] works (2)";
ok (    [==] 4, 4, 4),    "[==] works (1)";
ok (not [==] 4, 5, 4),    "[==] works (2)";
ok (    [!=] 4, 5, 6),    "[!=] works (1)";
ok (not [!=] 4, 4, 4),    "[!=] works (2)";

{
  my @array = (undef, undef, 3, undef, 5);
  is ([//]  *@array), 3, "[//] works";
  is ([err] *@array), 3, "[err] works";
}

{
  my @array = (undef, undef, 0, 3, undef, 5);
  is ([||] *@array), 3, "[||] works";
  is ([or] *@array), 3, "[or] works";
}

{
  is ([~] <a b c d>), "abcd", "[~] works";
}

{
  my $hash = {a => {b => {c => {d => 42, e => 23}}}};
  is eval('[.{}] $hash, <a b c d>'), 42, '[.{}] works', :todo<bug>;
}

{
  # 18:45 < autrijus> hm, I found a way to easily do linked list consing in Perl6
  # 18:45 < autrijus> [=>] 1..10;
  my $list = [=>] 1,2,3;
  is $list.key,         1, "[=>] works (1)";
  is $list.value.key,   2, "[=>] works (2)";
  is $list.value.value, 3, "[=>] works (3)";
}

{
  my @array = <5 -3 7 0 1 -9>;
  is eval('[,] @array'), @array, "[,] works (a noop)";
}


# Following two tests taken verbatim from former t/operators/reduce.t
eval_ok('my @foo = [1..3] >>+<< [1..3] >>+<< [1..3];','Sanity Check');
eval_ok('my @foo = [>>+<<] ([1..3],[1..3],[1..3]);','Parse [>>+<<]', :todo<bug>);

# Check that user defined infix ops work with [...], too.
sub infix:<more_than_plus>(Int $a, Int $b) { $a + $b + 1 }
is ([more_than_plus] 1, 2, 3), 8, "[...] reduce metaop works on user defined ops";
