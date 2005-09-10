#!/usr/bin/pugs

use Test;
use v6;

plan 15;

# L<S06/"The C<want> function" /or has the corresponding methods called on it:/>
sub obj_ok_in_item   { want.Item         ?? 42 !! 0 }
sub obj_ok_in_list   { want.List         ?? 42 !! 0 }
sub obj_ok_in_count2 { (want.count == 2) ?? 42 !! 0 }
sub obj_ok_in_count3 { (want.count == 3) ?? 42 !! 0 }

# ok_in_rw is different, because it has to be a lvalue.
sub obj_ok_in_rw is rw {
  my $forty_two = 42;
  my $zero      = 0;

  # By returning variables instead of constants, our sub can act as a lvalue.
  want.count ?? $forty_two !! $zero;
}

is try { my $item_ctx = obj_ok_in_item() }, 42,
  "want() works correctly in Item context (object-form)", :todo<feature>;

is try { my @list_ctx = obj_ok_in_list() },   42,
    "want() works correctly in List context (object-form)", :todo<feature>;

my ($a, $b, $c, $d, $e);
is try { ($a, $b)    = obj_ok_in_count2() }, 42,
  "want.count() works correct if two return values are expected (object-form)", :todo<feature>;

is try { ($c,$d,$e)  = obj_ok_in_count3() }, 42,
  "want.count() works correct if three return values are expected (object-form)", :todo<feature>;

is try { obj_ok_in_rw() = 23 },              42,
  "want() works correctly in rw context (object-form)", :todo<feature>;



# The same again, but this time using the smartmatch operator.
# L<S06/"The C<want> function" /typically tested with a smart match/>
sub sm_ok_in_item   { want ~~ 'Item' ?? 42 !! 0 }
sub sm_ok_in_list   { want ~~ 'List' ?? 42 !! 0 }
sub sm_ok_in_count2 { want ~~ 2      ?? 42 !! 0 }
sub sm_ok_in_count3 { want ~~ 3      ?? 42 !! 0 }

my ($item_ctx, @list_ctx);

is try { $item_ctx = sm_ok_in_item() }, 42,
  "want() works correctly in Item context (smartmatch-form)", :todo<feature>;

is try { @list_ctx   = sm_ok_in_list() },   42,
    "want() works correctly in List context (smartmatch-form)", :todo<feature>;

is try { ($a, $b)    = sm_ok_in_count2() }, 42,
  "want.count() works correct if two return values are expected (smartmatch-form)", :todo<feature>;
is try { ($c,$d,$e)  = sm_ok_in_count3() }, 42,
  "want.count() works correct if three return values are expected (smartmatch-form)", :todo<feature>;
  
# Test the identity of want() across function calls:
sub wants_array( @got ) { return @got };
sub gives_array() { return want };
my @a = gives_array;
@a = wants_array( @a );
my @b = wants_array(gives_array());
is( substr(@a, 0, 4), substr(@b, 0, 4), "want() context propagates consistently" ); 
like( @a[0], rx:P5/Item/, "The context is Item", :todo<bug> );
like( @b[0], rx:P5/Item/, "... on both subs", :todo<bug> );

# Test the identity again, via splice(), a builtin:
sub wants_array( @got ) { return @got };
my @tmp = (1..10);
@a = splice(@tmp, 8, 1);
@tmp = (1..10);
@b = wants_array(splice @tmp, 8, 1);
is( @a, @b, "want() results are consistent for builtins" ); 
is( @a, [9], "We got the expected results");
is( @b, [9], "... on both calls" );
