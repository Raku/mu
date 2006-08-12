use v6-alpha;

use Test;

plan 6;

# L<S04/"Closure traits" /at compile time, ALAP/>
# CHECK {...} block in "void" context
{
  my $str;
  BEGIN { $str ~= "begin1 "; }
  CHECK { $str ~= "check "; }
  BEGIN { $str ~= "begin2 "; }

  is $str, "begin1 begin2 check ", "check blocks run after begin blocks";
}

{
  my $str;
  CHECK { $str ~= "check1 "; }
  BEGIN { $str ~= "begin "; }
  CHECK { $str ~= "check2 "; }
  
  is $str, "begin check2 check1 ", "check blocks run in reverse order";
}

# CHECK {...} blocks as rvalues
{
  my $var;
  my $was_in_check;

  BEGIN { $var = 42 }
  $var = 19;
  my $a = { CHECK { $was_in_check++; $var } };

  is $a(), 42, 'our CHECK {...} block returned the correct var (1)';
  is $a(), 42, 'our CHECK {...} block returned the correct var (2)';
  is $a(), 42, 'our CHECK {...} block returned the correct var (3)';
  is $was_in_check, 1, 'our rvalue CHECK {...} block was executed exactly once';
}
