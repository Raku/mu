#!/usr/bin/pugs

use v6;
require Test;

plan 17;

{
	# basic sanity
	my ($t, $f);

	try { given 1 { when 1 { $t = 1 } } };
        ok($t, "given when true ...");

        try { given 1 { when 2 { $f = 1 } } };;
	todo_ok(!$f, "given when false");
};

{
	# simple case, with fall through
	my ($two, $five, $int, $unreached);
	eval 'given 5 {
		when 2 { $two = 1 }
		when 5 { $five = 1; next }
		when Int { $int = 1 }
		when 5 { $unreached = 1 }
	}';

	ok(!$two, "5 is not two");
	todo_ok($five, "5 is five");
	todo_ok($int, "short fell-through to next true when using 'next'");
	ok(!$unreached, "but didn't do so normally");
};

{
	my $foo;
	eval 'given "foo" { when "bar", /foo/ { $foo = 1 } }';

	todo_ok($foo, "foo was found in OR when");
};


# from apocalypse 4
{
	# simple example
	my ($result_a, $result_b);

	for ("T", "E", 5) -> $digit { # FIXME make this into: for zip(("T", "E", 5), (10, 11, 12)) -> $digit, $expected
		eval '$result_b = given $digit {
			when "T" { 10 }
			when "E" { 11 }
			$digit
		}';

		eval '$result_b = given $digit {
			when "T" { 10 }
			when "E" { 11 }
			default  { $digit }
		}';

		my $expected;
		
		if ($digit eq ( "T" | "E" )){
                	# $expected = ($digit eq "T") ?? 10 :: 11;
			if $digit eq "T" {
				$expected = 10;
			} else {
				$expected = 11;
			}
		} else {
			$expected = $digit;
		}

		todo_is($result_a, $expected, "result using implicit default {} is $expected");
		todo_is($result_b, $expected, "result using explicit default {} is $expected");
	};
};

{
	# interleaved code
	my ($b_one, $b_two, $b_three, $panic);
	eval '
	given 2 {
		$b_one = 1;
		when 1 { }
		$b_two = 1;
		when 2 { }
		$b_three = 1;
		default { }
		$panic = 1;
	}';

	todo_ok($b_one, "inteleraved 1");
	todo_ok($b_two, "inteleraved 2 is the last one");
	ok(!$b_three, "inteleraved 3 not executed");
	ok(!$panic, "never ever execute something after a default {}");
};

