#!/usr/bin/pugs

use v6;
require Test;

plan 4;

use_ok('Benchmark');


{
    lives_ok {
    my @a = (1,2,3);
    my @b = (4,5,6);
    timethese(100, { <hyper> => sub { my @r = @a >>+<< @b },
		     <normal> => sub {
			 my @r;
			 for (0..2) {
			     push @r, @a[$_] + @b[$_];
			 }
		     }
		   });
    }
}


{
    lives_ok {
	timethese(100, { <one_plus_one> => '1+1',
			 <two_plus_two> => '2+2',
		       });
    }
}

{
    lives_ok {
	timethese(10000, { <one_plus_one> => -> { 1 + 1},
			  <two_plus_two> => -> { 2 + 2},
			});
    }
}
