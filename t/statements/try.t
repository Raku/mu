#!/usr/bin/pugs

use v6;
require Test;

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

	todo_ok($caught, "exception caught");
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
	todo_ok($caught, "caught exception of class Naughty");
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
	todo_ok($naughty, "caught superclass");
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

	todo_ok(!$lived, "did not live past uncaught throw in try");
        ok(ref($!), '$! is an object');
	todo_is(eval 'ref($!)', "Dandy", ".. of the right class");
};
