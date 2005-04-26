#!/usr/bin/pugs

use Test;
use v6;

plan 16;

# L<S06/"The C<want> function" /or has the corresponding methods called on it:/>
sub obj_ok_in_scalar { want.Scalar       ?? 42 :: 0 }
sub obj_ok_in_list   { want.List         ?? 42 :: 0 }
sub obj_ok_in_count2 { (want.count == 2) ?? 42 :: 0 }
sub obj_ok_in_count3 { (want.count == 3) ?? 42 :: 0 }

# ok_in_rw is different, because it has to be a lvalue.
sub obj_ok_in_rw {
  my $forty_two = 42;
  my $zero      = 0;

  # By returning variables instead of constants, our sub can act as a lvalue.
  want.count ?? $forty_two :: $zero;
}

eval_is 'my $scalar_ctx = obj_ok_in_scalar()', 42,
  "want() works correctly in Scalar context (object-form)", :todo(1);

eval_is 'my @list_ctx   = obj_ok_in_list()',   42,
    "want() works correctly in List context (object-form)", :todo(1);

my ($a, $b, $c, $d, $e);
eval_is '($a, $b)    = obj_ok_in_count2()', 42,
  "want.count() works correct if two return values are expected (object-form)", :todo(1);

eval_is '($c,$d,$e)  = obj_ok_in_count3()', 42,
  "want.count() works correct if three return values are expected (object-form)", :todo(1);

eval_is 'obj_ok_in_count3() = 23',             42,
  "want() works correctly in rw context (object-form)", :todo(1);



# The same again, but this time using the smartmatch operator.
# L<S06/"The C<want> function" /typically tested with a smart match/>
sub sm_ok_in_scalar { want ~~ 'Scalar' ?? 42 :: 0 }
sub sm_ok_in_list   { want ~~ 'List'   ?? 42 :: 0 }
sub sm_ok_in_count2 { want ~~ 2        ?? 42 :: 0 }
sub sm_ok_in_count3 { want ~~ 3        ?? 42 :: 0 }

my ($scalar_ctx, @list_ctx);

eval_is '$scalar_ctx = sm_ok_in_scalar()', 42,
  "want() works correctly in Scalar context (smartmatch-form)", :todo(1);

eval_is '@list_ctx   = sm_ok_in_list()',   42,
    "want() works correctly in List context (smartmatch-form)", :todo(1);
fail "want() works correctly in List context (smartmatch-form)", :todo(1);

eval_is '($a, $b)    = sm_ok_in_count2()', 42,
  "want.count() works correct if two return values are expected (smartmatch-form)", :todo(1);
eval_is '($c,$d,$e)  = sm_ok_in_count3()', 42,
  "want.count() works correct if three return values are expected (smartmatch-form)", :todo(1);
  
# Test the identity of want() across function calls:
sub wants_array( @got ) { return @got };
sub gives_array() { return want };
my @a = gives_array;
@a = wants_array( @a );
my @b = wants_array(gives_array());
is( @a, @b, "want() context propagates consistently" ); 
is( @a[0], 'List', "The context is List" );
is( @b[0], 'List', "... on both subs" );

# Test the identity again, via splice(), a builtin:
sub wants_array( @got ) { return @got };
my @tmp = (1..10);
@a = splice @tmp, 8, 1;
@a = wants_array( @a );
@b = wants_array(splice @tmp, 8, 1);
is( @a, @b, "want() results are consistent for builtins" ); 
is( @a, [9], "We got the expected results");
is( @b, [9], "... on both calls" );
