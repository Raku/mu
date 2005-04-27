#!/usr/bin/pugs

use v6;
use Test;

plan 9;

{
	# simple try
	my $lived = undef;
	try { die "foo" };
	is($!, "foo", "error var was set");
};

{
	# try with a catch
	my $caught;
	eval 'try {
		die "blah"

		CATCH /la/ { $caught = 1 }
	}';

	ok($caught, "exception caught", :todo);
};


{
	# exception classes
	eval 'class Naughty is Exception {}';

	my ($not_died, $caught);
	eval 'try {
		die Naughty "error"

		$not_died = 1;

		CATCH Naughty {
			$caught = 1;
		}
	}';

	ok(!$not_died, "did not live after death");
	ok($caught, "caught exception of class Naughty", :todo);
};

{
	# exception superclass
	eval 'class Naughty::Specific is Naughty {}';
	eval 'class Naughty::Other is Naughty {}';

	my ($other, $naughty);
	eval 'try {
		die Naughty::Specific "error";

		CATCH Naughty::Other {
			$other = 1;
		}
	
		CATCH Naughty {
			$naughty = 1;
		}
	}';

	ok(!$other, "did not catch sibling error class");
	ok($naughty, "caught superclass", :todo);
};

{
	# uncaught class
	eval 'class Dandy is Exception {}';

	my ($naughty, $lived);
	eval 'try {
			die Dandy "error";
		
			CATCH Naughty {
				$naughty = 1;
			}
		};

		$lived = 1;
	';

	ok(!$lived, "did not live past uncaught throw in try", :todo);
        ok(ref($!), '$! is an object');
	is(eval 'ref($!)', "Dandy", ".. of the right class", :todo);
};
