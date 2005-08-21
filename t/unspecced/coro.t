#!/usr/bin/pugs

use v6;
use Test;

plan 12;

# Standard function of fp
sub take(Int $n, Code &f) { (1..$n).map:{ f() } }

# Anonymous coroutines
{
  my $coro  = coro { yield 42; yield 23 };
  my @elems = take 5, $coro;
  is ~@elems, "42 23 42 23 42", "anonymous coroutines work";
}

# Named coroutines
{
  coro foo { yield 42; yield 23 };
  is ~take(5, &foo), "42 23 42 23 42", "named coroutines work";
}

# return() resets the entrypoint
{
  my $coro  = coro { yield 42; yield 23; return 13; yield 19 };
  my @elems = take 7, $coro;
  is ~@elems, "42 23 13 42 23 13 42", "return() resets the entrypoint";
}

# Coroutines stored in an array
{
  my @array = take 5, {
    coro {
      my $num;
      while 1 {
	yield ++$num;
      }
    };
  };

  is ~take(5, @array[0]), "1 2 3 4 5",  "coroutines stored in arrays work (1)";
  is ~take(5, @array[1]), "1 2 3 4 5",  "coroutines stored in arrays work (2)";
  is ~take(5, @array[0]), "6 7 8 9 10", "coroutines stored in arrays work (3)";
  is ~take(5, @array[2]), "1 2 3 4 5",  "coroutines stored in arrays work (4)";
  is ~take(5, @array[1]), "6 7 8 9 10", "coroutines stored in arrays work (5)";
}

# Test that there's still only one instance of each state() variable
{
  my @array = take 5, {
    coro {
      while 1 {
	state $num;
	yield ++$num;
      }
    };
  };

  is ~take(5, @array[0]),      "1 2 3 4 5", "state() in coroutines work (1)";
  is ~take(5, @array[1]),     "6 7 8 9 10", "state() in coroutines work (2)";
  is ~take(5, @array[0]), "11 12 13 14 15", "state() in coroutines work (3)";
}

# Test that there's still only one instance of each state() variable
try {
  my @array = take 5, {
    coro {
	(sub {
          while 1 {
	    state $num;
	    yield ++$num;
          }
        })();
    };
  };

  is ~take(5, @array[0]),      "1 2 3 4 5", "yield from inside closure";
};

# I've marked this failure as unspecced, should a yield be able to
# jump up many scopes like that?
ok $!, "yield() should work from inside a closure (unspecced!)";
