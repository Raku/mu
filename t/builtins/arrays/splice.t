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

plan 25;

my (@a,@b);

sub splice_ok (Array @got, Array @ref, Array @exp, Array @exp_ref, Str $comment) {
  is @got, @exp, "$comment - results match";
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

sub eval_splice_ok( Str $statement, Array @exp, Array @exp_ref, Str $comment) {
  skip 3, "Need to implement/fix eval_splice_ok";
  return 1;

  my (@a,@b);
  @b = eval $statement;

  ok "$comment - eval";
  #if ($parse_ok !~ 1) {
  #  fail "$comment - eval failed";
  #  skip 2, "$comment - eval failed";
  #} else {
    splice_ok @b, @a, @exp, @exp_ref, $comment;
  #};
};

eval_ok '~splice(@a,@a,0,11,12) eq "" && ~@a eq ~[1..12]';
@a = (1..10);
# @b = splice(@a,+@a,0,11,12);
# is( @b, [], "push-via-splice result works" );
# is (@a, ([1..12]), "push-via-splice modification works");

# Bug 20000223.001 - no test for splice(@array).  Destructive test!
# todo_eval_ok '~splice(@a) eq ~[1,2,7,3] && ~@a eq \'\'';

my $foo;
@a   = ('red', 'green', 'blue');
todo_is( (eval 'splice @a, 1, 2'), "blue", "splice() in scalar context returns last element of list");

# Test the single arg form of splice (which should die IMO)
eval_splice_ok( '@a = (1..10); splice @a',[1..10],[], "Single-arg splice returns the whole list" );

@a = (1..10);
splice_ok( splice(@a,8,2), @a, [9,10], [1..8], "3-arg positive indices work");
# is @a, [1..8], "The array got modified properly";

@a = (1..12);
splice_ok splice(@a,0,1), @a, [1], [2..11], "Simple 3-arg splice";

eval_splice_ok '@a = (1..10); splice(@a,8)', [9,10], [1..8], "2-arg positive indices work";

@a = (1..10);
splice_ok splice(@a,-2,2), @a, [9,10], [1..8], "3-arg negative indices work";

@a = (1..10);
@b = ();
eval_splice_ok '@a=(1..10);splice(@a,-2)', [9,10], [1..8], "2-arg negative indices work";

# to be converted into more descriptive tests
# todo_eval_ok '~splice(@a,0,0,0,1) eq "" && ~@a eq ~[0..11]';
@a = (2..10);
eval_splice_ok '@a = (2..10); splice(@a,0,0,0,1)', [], [0..10], "Prepending values works";

# Need to convert these
skip 5, "Need to convert more tests from Perl5";
#todo_eval_ok '~splice(@a,5,1,5) eq "5" && ~@a eq ~[0..11]';
#todo_eval_ok '~splice(@a, @a, 0, 12, 13) eq "" && ~@a eq ~[0..13]';
#todo_eval_ok '~splice(@a, -@a, @a, 1, 2, 3) eq ~[0.13] && ~@a eq ~[1..3]';
#todo_eval_ok '~splice(@a, 1, -1, 7, 7) eq "2" && ~@a eq ~[1,7,7,3]';
#todo_eval_ok '~splice(@a,-3,-2,2) eq ~[7] && ~@a eq ~[1,2,7,3]';

