use v6-alpha;

use Test;

plan 7;

# L<S04/"Closure traits" /at run time, ASAP/>
# INIT {...} blocks in "void" context
{
  my $str;
  is $str, "begin1 begin2 init ", "init blocks run after begin blocks";

  BEGIN { $str ~= "begin1 "; }
  INIT  { $str ~= "init "; }
  BEGIN { $str ~= "begin2 "; }
}

{
  my $str;
  is $str, "check2 check1 init ", "init blocks run after check blocks";
  
  CHECK { $str ~= "check1 "; }
  INIT  { $str ~= "init "; }
  CHECK { $str ~= "check2 "; }
}

{
  my $str;
  is $str, "begin init1 init2 ", "init blocks run in forward order";
  
  INIT  { $str ~= "init1 "; }
  BEGIN { $str ~= "begin "; }
  INIT  { $str ~= "init2 "; }
}

# INIT {...} blocks as rvalues
{
  my $var;
  my $was_in_init;

  my $sub = { my $z = INIT { $was_in_init++; $var }; $z + 1 };
  BEGIN { $var = 42 }

  $var = 23;

  is $sub(), 43, 'our rvalue INIT {...} block returned the correct var (1)';
  is $sub(), 43, 'our rvalue INIT {...} block returned the correct var (2)';
  is $sub(), 43, 'our rvalue INIT {...} block returned the correct var (3)';
  is $was_in_init, 1, 'our rvalue INIT {...} block was executed exactly once';
}
