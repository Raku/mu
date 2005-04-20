#!/usr/bin/pugs

use v6;
require Test;

plan 47;

my $foo = "FOO";
my $bar = "BAR";

=kwid

Tests quoting constructs as defined in L<S02/Literals>

=todo

* q:t - heredocs
* q:0, q:b, and other interpolation levels
* meaningful quotations (qx, rx, etc)
* review shell quoting semantics of «»
* arrays in «»
* interpolation of scalar, array, hash, function and closure syntaxes
* q : a d verb s // parsing

=cut


{ # backslash interpolation only single quotes L<S02/Literals /:single\s+Interpolate \\\\,/>
	my @q = ();
	eval '@q = (q/$foo $bar/)';
	is(+@q, 1, 'q// is singular');
	is(@q[0], '$foo $bar', 'single quotes are non interpolating');
};

{ # and it's complement ;-)
	my @q = ();
	eval "\@q = '\$foo \$bar'";
	is(+@q, 1, "'' is singular");
	is(@q[0], '$foo $bar', 'and did not interpolate either');
};

{ # non interpolating single quotes with nested parens L<S02/Literals /That is.*?\(\).*?have no special significance/>
	my @q = ();
	eval '@q = (q: (($foo $bar)))';
	todo_is(+@q, 1, 'q: () is singular');
	todo_is(@q[0], '($foo $bar)', 'and nests parens appropriately');
};

{ # q() is bad L<S02/Literals /Which is mandatory for parens/>
	my @q = ();
	eval '@q = (q(($foo $bar)))';
	is(+@q, 0, 'nothing in @q, q() is not allowed');
};

{ # adverb variation L<S02/Literals /:1/>
	my @q = ();
	eval '@q = (q:1/$foo $bar/)';
	is(+@q, 1, "q:1// is singular");
	is(@q[0], '$foo $bar', "and again, non interpolating");
};


{ # interpolating quotes L<S02/Literals /same as qq/>
	my @q = ();
        @q = qq/$foo $bar/;
	is(+@q, 1, 'qq// is singular');
	is(@q[0], 'FOO BAR', 'variables were interpolated');
};

{ # "" variation
	my @q = ();
        @q = "$foo $bar";
	is(+@q, 1, '"" is singular');
	is(@q[0], "FOO BAR", '"" interpolates');
};

{ # adverb variation L<S02/Literals /:2/>
	my @q = ();
	eval '@q = q:2/$foo $bar/';
	is(+@q, 1, "q:2// is singular");
	is(@q[0], "FOO BAR", "blah blah interp");
};


{ # quote with \0 as delimiters L<news:20050101220112.GF25432@plum.flirble.org>
	my @q = ();
	eval "\@q = (q\0foo bar\0)";
	is(+@q, 1, "single quote with \\0 delims are parsed ok");
	is(@q[0], "foo bar", "and return correct value");
};


{ # traditional quote word
	my @q = ();
	eval '@q = (qw/$foo $bar/)';
	is(+@q, 2, "qw// is plural");
	is(@q[0], '$foo', "and non interpolating");
	is(@q[1], '$bar', "...");
};

{ # angle brackets L<S02/Literals /the qw.*?quote operator.*?bracketed form/>
	my @q = ();
	eval '@q = <$foo $bar>';
	is(+@q, 2, "<> behaves the same way");
	is(@q[0], '$foo', 'for interpolation too');
	is(@q[1], '$bar', '...');
};

{ # angle brackets L<S02/Literals /the qw.*?quote operator.*?bracketed form/>
	my @q = ();
	eval '@q = < $foo $bar >';
	is(+@q, 2, "<> behaves the same way, with leading (and trailing) whitespace");
	is(@q[0], '$foo', 'for interpolation too');
	is(@q[1], '$bar', '...');
};

{ # adverb variation
	my @q = ();
	eval '@q = (q:w/$foo $bar/)';
	is(+@q, 2, "q:w// is also identical");
	is(@q[0], '$foo', "...");
	is(@q[1], '$bar', "...");
};


{ # qw, interpolating L<S02/Literals /do not interpolate while double angles do/>
	my (@q1, @q2, @q3) = ();
	@q1 = q:ww/$foo gorch $bar/;
	@q2 = «$foo gorch $bar»; # french
	@q3 = <<$foo gorch $bar>>; # texas

	is(+@q1, 3, 'q:ww// correct number of elements');
	is(+@q2, 3, 'french double angle');
	is(+@q3, 3, 'texas double angle');

	todo_is(~@q1, "FOO gorch BAR", "explicit quote word interpolates");
	is(~@q2, "FOO gorch BAR", "output is the same as french,");
	is(~@q3, "FOO gorch BAR", "and texas quotes");
};

{ # qw, interpolating, shell quoting L<S02/Literals /respects quotes in a shell-like fashion/>
	my (@q1, @q2) = ();
	my $gorch = "foo bar";

	eval '@q1 = «$foo $gorch $bar»';
	is(+@q1, 4, "4 elements in unquoted «» list");
	is(@q1[2], "bar", '$gorch was exploded');
	is(@q1[3], "BAR", '$bar was interpolated');

	eval '@q2 = «$foo "$gorch" \'$bar\'»';
	todo_is(+@q2, 3, "3 elementes in sub quoted «» list");
	todo_is(@q2[1], $gorch, 'second element is both parts of $gorch, interpolated');
	todo_is(@q2[2], '$bar', 'single quoted $bar was not interpolated');
};

{ # qq:t L<S02/Literals /Heredocs are no longer written/>
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

{ # q:t indented L<S02/Literals /Here docs allow optional whitespace/>
	my @q = ();

	eval '@q = q:t/FOO/;
		blah blah
		$foo
		FOO
	';

	todo_is(+@q, 1, "q:t// is singular, also when indented");
	todo_is(@q[0], "blah blah\n\$foo\n", "indentation stripped");
};

{ # q:0 L<S02/Literals /No escapes at all/>
	my @q = ();
	
	my $backslash = "\\";

	eval '@q = (q:0/foo' ~ $backslash ~ $backslash ~ 'bar$foo/)';

	is(+@q, 1, "q:0// is singular");
	is(@q[0], "foo\\\\bar\$foo", "special chars are meaningless"); # double quoting is to be more explicit
};
