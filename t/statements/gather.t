#!/usr/bin/pugs

use v6;
use Test;

plan 7;

# Standard gather
{
	my @a;
	my $i;
	
	@a = gather {
		$i = 1;
		for (1 .. 5) -> $j {
			take $j;
		}
	};

	ok(!$i, "not yet gathered", :todo<unspecced>);
	is(+@a, 5, "5 elements gathered");
	ok($i, "gather code executed");
	is(@a[0], 1, "first elem taken");
	is(@a[-1], 5, "last elem taken");
};

# Nested gathers, two levels
{
  my @outer = gather {
    for 1..3 -> $i {
      my @inner = gather {
	take "$i,$_" for 1..3;
      };

      take ~@inner;
    }
  };

  is ~@outer, "1,1 1,2 1,3 2,1 2,2 2,3 3,1 3,2 3,3", "nested gather works (two levels)";
}

# Nested gathers, three levels
{
  my @outer = gather {
    for 1..2 -> $i {
      my @inner = gather {
	for 1..2 -> $j {
	  my @inner_inner = gather {
	    take "$i,$j,$_" for 1..2;
	  };
	  take ~@inner_inner;
	}
      };
      take ~@inner;
    }
  };

  is ~@outer, "1,1,1 1,1,2 1,2,1 1,2,2 2,1,1 2,1,2 2,2,1 2,2,2", "nested gather works (three levels)";
}
