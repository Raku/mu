#!/usr/bin/pugs

use v6;
require Test;

plan 44;

my $foo = "FOO";
my $bar = "BAR";

=todo

- q:t - heredocs
- q:0, q:b, and other interpolation levels
- meaningful quotations (qx, rx, etc)
- review shell quoting semantics of «»
- arrays in «»
- interpolation of scalar, array, hash, function and closure syntaxes
- q : a d verb s // parsing

=cut


{ # non interpolating single quotes
	my @q = ();
	eval '@q = (q/$foo $bar/)';
	todo_is(+@q, 1, 'q// is singular');
	todo_is(@q[0], '$foo $bar', 'single quotes are non interpolating');
};

{ # and it's complement ;-)
	my @q = ();
	eval "\@q = '\$foo \$bar'";
	is(+@q, 1, "'' is singular");
	is(@q[0], '$foo $bar', 'and did not interpolate either');
};

{ # non interpolating single quotes with nested parens
	my @q = ();
	eval '@q = (q:(($foo $bar)))';
	todo_is(+@q, 1, 'q:() is singular');
	todo_is(@q[0], '($foo $bar)', 'and nests parens appropriately');
};

{ # q() is bad
	my @q = ();
	eval '@q = (q(($foo $bar)))';
	is(+@q, 0, 'nothing in @q, q() is not allowed');
};

{ # adverb variation
	my @q = ();
	eval '@q = (q:1/$foo $bar/)';
	todo_is(+@q, 1, "q:1// is singular");
	todo_is(@q[0], '$foo $bar', "and again, non interpolating");
};


{ # interpolating quotes
	my @q = ();
	eval '@q = (qq/$foo $bar/)';
	is(+@q, 1, 'qq// is singular');
	is(@q[0], 'FOO BAR', 'variables were interpolated');
};

{ # "" variation
	my @q = ();
	eval '@q = ("$foo $bar")';
	is(+@q, 1, '"" is singular"');
	is(@q[0], "FOO BAR", '"" interpolates');
};

{ # adverb variation
	my @q = ();
	eval '@q = q:2/$foo $bar/';
	todo_is(+@q, 1, "q:2// is singular");
	todo_is(@q[0], "FOO BAR", "blah blah interp");
};


{ # quote with \0 as delimiters
	my @q = ();
	eval "\@q = (q\0foo bar\0)";
	todo_is(+@q, 1, "single quote with \\0 delims are parsed ok");
	todo_is(@q[0], "foo bar", "and return correct value");
};


{ # traditional quote word
	my @q = ();
	eval '@q = (qw/$foo $bar/)';
	is(+@q, 2, "qw// is plural");
	is(@q[0], '$foo', "and non interpolating");
	is(@q[1], '$bar', "...");
};

{ # angle brackets
	my @q = ();
	eval '@q = <$foo $bar>';
	is(+@q, 2, "<> behaves the same way");
	is(@q[0], '$foo', 'for interpolation too');
	is(@q[1], '$bar', '...');
};

{ # adverb variation
	my @q = ();
	eval '@q = (q:w/$foo $bar/)';
	todo_is(+@q, 2, "q:w// is also identical");
	todo_is(@q[0], '$foo', "...");
	todo_is(@q[1], '$bar', "...");
};


{ # qw, interpolating
	my (@q1, @q2, @q3) = ();
	eval '@q1 = q:ww/$foo gorch $bar/';
	eval '@q2 = «$foo gorch $bar»'; # french
	eval '@q3 = <<$foo gorch $bar>>'; # texas

	todo_is(+@q1, 3, 'q:ww// correct number of elements');
	todo_is(+@q2, 3, 'french double angle');
	todo_is(+@q3, 3, 'texas double angle');

	todo_is(~@q1, "FOO gorch BAR", "explicit quote word interpolates");
	is(~@q2, ~@q1, "output is the same as french,");
	is(~@q3, ~@q1, "and texas quotes");
};

{ # qw, interpolating, shell quoting
	my (@q1, @q2) = ();
	my $gorch = "foo bar";

	eval '@q1 = «$foo $gorch $bar»';
	todo_is(+@q1, 4, "4 elements in unquoted «» list");
	todo_is(@q1[2], "bar", '$gorch was exploded');
	todo_is(@q1[3], "BAR", '$bar was interpolated');
	
	eval '@q2 = «$foo "$gorch" \'$bar\'»';
	todo_is(+@q2, 3, "3 elementes in sub quoted «» list");
	todo_is(@q2[1], $gorch, 'second element is both parts of $gorch, interpolated');
	todo_is(@q2[2], '$bar', 'single quoted $bar was not interpolated');
};

{ # qq:t
	my @q = ();
	
	eval '@q = qq:t/FOO/;
blah
$bar
blah
$foo
FOO
	';

	todo_is(+@q, 1, "q:t// is singular");
	todo_is(@q[0], "blah\nBAR\nblah\nFOO\n", "here doc interpolated");
};

{ # q:t indented
	my @q = ();

	eval '@q = q:t/FOO/;
		blah blah
		$foo
		FOO
	';

	todo_is(+@q, 1, "q:t// is singular, also when indented");
	todo_is(@q[0], "blah blah\n\$foo\n", "indentation stripped");
};

{ # q:0
	my @q = ();

	eval '@q = (q:0/foo\\bar$foo/)';

	todo_is(+@q, 1, "q:0// is singular");
	todo_is(@q[0], "foo\\\\bar\$foo", "special chars are meaningless"); # double quoting is to be more explicit
};

