#!/usr/bin/pugs

use Test;
use v6;

=pod
=head1 DESCRIPTION

This test tests the C<reduce> builtin and the reduce metaoperator C<[...]>.

References:
L<http://groups.google.com/groups?selm=420DB295.3000902%40conway.org> and
L<http://groups.google.de/group/perl.perl6.language/msg/bd9eb275d5da2eda>

=cut

plan 35;

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

# Reduce with n-ary functions
{
  my @array  = <1 2 3 4 5 6 7 8>;
  my $result = (((1 + 2 * 3) + 4 * 5) + 6 * 7) + 8 * undef;

  is @array.reduce:{ $^a + $^b * $^c }, $result, "n-ary reduce() works";
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
  my $hash = {a => {b => 42}};
  is ([.{}] $hash, <a b>), 42, '[.{}] works two levels deep';
}

{
  my $hash = {a => {b => {c => {d => 42, e => 23}}}};
  is eval('[.{}] $hash, <a b c d>'), 42, '[.{}] works', :todo<bug>;

  my $arr = [[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]];
  is ([.[]] $arr, 1, 0, 2), 9, '[.[]] works';
}

{
  my $hash = {a => {b => {c => 42}}};
  my @reftypes;
  sub foo (Hash $hash, String $key) {
    push @reftypes, $hash.ref;
    $hash.{$key};
  }
  is((reduce(&foo, $hash, <a b c>)), 42, 'reduce(&foo) (foo ~~ .{}) works three levels deep');
  is(@reftypes[0], "Hash", "first application of reduced hash subscript passed in a Hash"); # Array
  is(@reftypes[1], "Hash", "second application of reduced hash subscript passed in a Hash"); # Scalar::Proxy
  is(@reftypes[2], "Hash", "third application of reduced hash subscript passed in a Hash"); # Scalar::Proxy
}

{
  # 18:45 < autrijus> hm, I found a way to easily do linked list consing in Perl6
  # 18:45 < autrijus> [=>] 1..10;
  my $list = [=>] 1,2,3;
  is $list.key,         1, "[=>] works (1)";
  is try{$list.value.key},   2, "[=>] works (2)", :todo<bug>;
  is try{$list.value.value}, 3, "[=>] works (3)", :todo<bug>;
}

{
  my @array = <5 -3 7 0 1 -9>;
  is eval('[,] @array'), @array, "[,] works (a noop)";
}


# Following two tests taken verbatim from former t/operators/reduce.t
eval_ok('my @foo = [1..3] >>+<< [1..3] >>+<< [1..3];','Sanity Check');
eval_ok('my @foo = [>>+<<] ([1..3],[1..3],[1..3]);','Parse [>>+<<]');

# Check that user defined infix ops work with [...], too.
sub infix:<more_than_plus>(Int $a, Int $b) { $a + $b + 1 }
is([more_than_plus] 1, 2, 3), 8, "[...] reduce metaop works on user defined ops";
