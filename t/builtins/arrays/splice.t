#!/usr/bin/pugs

require Test;
use v6;

=head1 DESCRIPTION

This test tests the C<splice> builtin, see S29 and Perl 5's perlfunc.

Ported from the equivalent Perl 5 test.

This test includes a test for the single argument form of
C<splice>. Depending on whether the single argument form
of C<splice> should survive or not, this test should be dropped.

  my @a = (1..10);
  splice @a;

is equivalent to:

  my @a = (1..10);
  @a = ();

=cut

plan 27;

my (@a,@b);

sub splice_ok (Array @got, Array @ref, Array @exp, Array @exp_ref, Str $comment) {
  is "[@got[]]", "[@exp[]]", "$comment - results match";
  is @ref, @exp_ref, "$comment - array got modified in-place";

  # Once we get Test::Builder, this will be better:
  #if ( (@got ~~ @exp) and (@ref ~~ @exp_ref)) {
  #  fail($comment);
  #  if (@got !~ @exp) {
  #    diag "The returned result is wrong:";
  #    diag "  Expected: @exp";
  #    diag "  Got     : @got";
  #  };
  #  if (@ref !~ @exp_ref) {
  #    diag "The modified array is wrong:";
  #    diag "  Expected: @exp_ref";
  #    diag "  Got     : @exp";
  #  };
  #} else {
  #  ok($comment);
  #};
};

@a = (1..10);
@b = splice(@a,+@a,0,11,12);

is( @b, [], "push-via-splice result works" );
is (@a, ([1..12]), "push-via-splice modification works");

@a  = ('red', 'green', 'blue');
is( splice(@a, 1, 2), "blue", "splice() in scalar context returns last element of list");

# Test the single arg form of splice (which should die IMO)
@a = (1..10);
splice_ok( splice(@a), @a, [1..10],[], "Single-arg splice returns the whole list" );

@a = (1..10);
splice_ok( splice(@a,8,2), @a, [9,10], [1..8], "3-arg positive indices work");
# is @a, [1..8], "The array got modified properly";

@a = (1..12);
splice_ok splice(@a,0,1), @a, [1], [2..12], "Simple 3-arg splice";

@a = (1..10);
splice_ok splice(@a,8), @a, [9,10], [1..8], "2-arg positive indices work";

@a = (1..10);
splice_ok splice(@a,-2,2), @a, [9,10], [1..8], "3-arg negative indices work";

@a = (1..10);
splice_ok splice(@a,-2), @a, [9,10], [1..8], "2-arg negative indices work";

# to be converted into more descriptive tests
# todo_eval_ok '~splice(@a,0,0,0,1) eq "" && ~@a eq ~[0..11]';
@a = (2..10);
splice_ok splice(@a,0,0,0,1), @a, [], [0..10], "Prepending values works";

# Need to convert these
# skip 5, "Need to convert more tests from Perl5";
@a = (0..11);
splice_ok splice(@a,5,1,5), @a, [5], [0..11], "Replacing an element with itself";

@a = (0..11);
splice_ok splice(@a, @a, 0, 12, 13), @a, [], [0..13], "Appending a list";

@a = (0..13);
splice_ok splice(@a, -@a, @a, 1, 2, 3), @a, [0..13], [1..3], "Replacing the array contents from right end";

@a = (1, 2, 3);
splice_ok splice(@a, 1, -1, 7, 7), @a, [2], [1,7,7,3], "Replacing a list into the middle";

@a = (1,7,7,3);
splice_ok splice(@a,-3,-2,2), @a, [7], [1,2,7,3], "Replacing negative count of elements";
