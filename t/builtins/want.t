#!/usr/bin/pugs

require Test;
use v6;

plan 10;

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

todo_eval_is 'my $scalar_ctx = obj_ok_in_scalar()', 42,
  "want() works correctly in Scalar context (object-form)";

todo_eval_is 'my @list_ctx   = obj_ok_in_list()',   42,
    "want() works correctly in List context (object-form)";

my ($a, $b, $c, $d, $e);
todo_eval_is '($a, $b)    = obj_ok_in_count2()', 42,
  "want.count() works correct if two return values are expected (object-form)";

todo_eval_is '($c,$d,$e)  = obj_ok_in_count3()', 42,
  "want.count() works correct if three return values are expected (object-form)";

todo_eval_is 'obj_ok_in_count3() = 23',             42,
  "want() works correctly in rw context (object-form)";



# The same again, but this time using the smartmatch operator.
# L<S06/"The C<want> function" /typically tested with a smart match/>
sub sm_ok_in_scalar { want ~~ 'Scalar' ?? 42 :: 0 }
sub sm_ok_in_list   { want ~~ 'List'   ?? 42 :: 0 }
sub sm_ok_in_count2 { want ~~ 2        ?? 42 :: 0 }
sub sm_ok_in_count3 { want ~~ 3        ?? 42 :: 0 }

my ($scalar_ctx, @list_ctx);

todo_eval_is '$scalar_ctx = sm_ok_in_scalar()', 42,
  "want() works correctly in Scalar context (smartmatch-form)";

todo_eval_is '@list_ctx   = sm_ok_in_list()',   42,
    "want() works correctly in List context (smartmatch-form)";
todo_fail "want() works correctly in List context (smartmatch-form)";

todo_eval_is '($a, $b)    = sm_ok_in_count2()', 42,
  "want.count() works correct if two return values are expected (smartmatch-form)";
todo_eval_is '($c,$d,$e)  = sm_ok_in_count3()', 42,
  "want.count() works correct if three return values are expected (smartmatch-form)";
