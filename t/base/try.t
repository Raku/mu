#!/usr/bin/pugs

use v6;
require Test;

plan 10;

{
	# simple try
	my $lived = undef;
	eval 'try { die "foo" }; $lived = 1';
	todo_is(eval '$!', "foo", "error var was set");
	ok($lived, "try weakened death");
};

{
	# try with a catch
	my $cought;
	eval 'try {
		die "blah"

		CATCH /la/ { $cought = 1 }
	}';

	todo_ok($cought, "exception cought");
};


{
	# exception classes
	eval 'class Naughty is Exception {}';

	my ($not_died, $cought);
	eval 'try {
		die Naughty "error"

		$not_died = 1;

		CATCH Naughty {
			$cought = 1;
		}
	}';

	ok(!$not_died, "did not live after death");
	todo_ok($cought, "cought exception of class Naughty");
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
	todo_ok($naughty, "cought superclass");
};

{
	# uncought class
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

	todo_ok(!$lived, "did not lived passed uncought throw in try");
        ok(ref($!), '$! is an object');
	todo_is(eval 'ref($!)', "Dandy", ".. of the right class");
};
