#!/usr/bin/pugs

use v6;
require Test;

=kwid

"undef" tests

This test file contains two sections: a port of the perl5 undef.t tests,
and perl6-specific tests.

=cut

plan 72;

our $GLOBAL;

is(undef, undef, "undef is equal to undef");
ok(!defined(undef), "undef is not defined");

{
	my $a;
	is($a, undef, "uninitialized lexicals are undef");

	is($GLOBAL, undef, "uninitialized globals are undef");

	$a += 1; # should not emit a warning. how to test that?
	ok(defined($a), "initialized var is defined");

	undef $a;
	ok(!defined($a), "undef($a) does");

	$a = "hi";
	ok(defined($a), "string");

	my $b;
	$a = $b;
	ok(!defined($a), "assigning another undef lexical");

	$a = $GLOBAL;
	ok(!defined($a), "assigning another undef global");
}

{
	my @ary = "arg1";
	my $a = @ary.pop;
	ok(defined($a), "pop from array");
	$a = @ary.pop;
	ok(!defined($a), "pop from empty array");

	@ary = "arg1";
	$a = @ary.shift;
	ok(defined($a), "shift from array");
	$a = @ary.shift;
	ok(!defined($a), "shift from empty array");

	my %hash = ( bar => 'baz', quux => 'quuz' );
	ok(defined(%hash{"bar"}), "hash subscript");
	todo_ok(eval'!defined(%hash{"bargho"})', "non-existent hash subscript") or
		diag("expected undef; got { %hash{'bargho'} }");

	todo_fail("FIXME parsefail"); # currently fails compilation even in eval
	#eval 'undef %hash{"bar"}';
	#todo_ok(!defined(%hash{"bar"}), "undef hash subscript");

	eval '
		%hash{"bar"} = "baz";
		delete %hash{"bar"};
	';
	todo_ok(!defined(%hash{"bar"}), "delete hash subscript");

	ok(defined(@ary), "aggregate array defined");
	ok(defined(%hash), "aggregate hash defined");

	undef @ary;
    ok(!defined(@ary), "undef array");

	undef %hash;
        ok(!defined(%hash), "undef hash");

	@ary = (1);
	ok(defined(@ary), "define array again");
	%hash = (1,1);
	ok(defined(%hash), "define hash again");
}

{
	# rjbs reported this bug:
	ok(eval 'my %hash; %hash = {}; undef %hash; %hash');
}

{
	sub a_sub { "møøse" }

	ok(defined(&a_sub), "defined sub");
	todo_ok(eval 'defined(%«$?PACKAGE\::»<&a_sub>)',
			"defined sub (symbol table)");

	ok(eval '!defined(&a_subwoofer)', "undefined sub"); # unTODOme
	todo_ok(eval '!defined(%«$?PACKAGE\::»<&a_subwoofer>)',
			"undefined sub (symbol table)");
}

# TODO: find a read-only value to try and assign to, since we don't
# have rules right now to play around with (the p5 version used $1)
#eval { "constant" = "something else"; };
#is($!, "Modification of a read", "readonly write yields exception");

# skipped tests for tied things

# skipped test for attempt to undef a bareword -- no barewords here.

# TODO: p5 "bugid 3096
# undefing a hash may free objects with destructors that then try to
# modify the hash. To them, the hash should appear empty."


# Test LHS assignment to undef:

my $interesting;
eval_ok( '(undef, undef, $interesting) = (1,2,3)',"Undef on LHS of list assignment");
is($interesting,3, "Undef on LHS of list assignment");

eval_ok('(undef, $interesting, undef) = (1,2,3)', "Undef on LHS of list assignment");
is($interesting,2, "Undef on LHS of list assignment");

eval_ok('($interesting, undef, undef) = (1,2,3)', "Undef on LHS of list assignment");
is($interesting,1, "Undef on LHS of list assignment");

sub two_elements() { (1,2) };
eval_ok( '(undef,$interesting) = two_elements();', "Undef on LHS of function assignment");
is($interesting,2, "Undef on LHS of function assignment");
eval_ok( '($interesting, undef) = two_elements();', "Undef on LHS of function assignment");
is($interesting,1, "Undef on LHS of function assignment");

=kwid

Perl6-specific tests

=cut

{
	# aggregate references

	my @ary = (<a b c d e>);
	my $ary_r = @ary; # ref
	isa_ok($ary_r, "Array");
	ok(defined($ary_r), "array reference");

	#undef @ary;
	#ok(defined($ary_r), "undef array referent");
	fail 'FIXME: parsefail undef @ary: cannot modify a constant item';

	is(+$ary_r, 0, "dangling array reference"); # unTODOme

	my %hash = (1, 2, 3, 4);
	my $hash_r = %hash;
	isa_ok($hash_r, "Hash");
	ok(defined($hash_r), "hash reference");
	#undef %hash;
	#ok(defined($hash_r), "undef hash referent:");
	fail 'FIXME: parsefail undef %hash: cannot modify a constant item';
	is(+$hash_r.keys, 0, "dangling hash reference"); # unTODOme
}

{
	# types
	# TODO: waiting on my Dog $spot;

	eval 'my Array $an_ary';
	todo_ok(eval '!defined($an_ary)', "my Array");
	todo_ok(eval '!defined($an_ary.0)', "my Array subscript - undef");
	eval '$an_ary.push("blergh")';
	todo_ok(eval 'defined($an_ary.pop)', "push");
	todo_ok(eval '!defined($an_ary.pop)', "comes to shove");

	eval 'my Hash $a_hash';
	todo_ok(eval '!defined($a_hash)', "my Hash");
	todo_ok(eval '!defined($a_hash{"blergh"})', "my Hash subscript - undef");
	todo_ok(eval '!defined($a_hash{"blergh"})', "my Hash subscript - undef, even after autovivification");
	eval '$a_hash{"blergh"} = 1';
	todo_ok(eval 'defined($a_hash{"blergh"}.delete)', "delete");
	todo_ok(eval '!defined($a_hash{"blergh"}.delete)', " - once only");

	eval '
		class Dog {};
		my Dog $spot;
	';

	todo_ok(eval '!defined $spot', "Unelaborated mutt");
	eval '$spot .= .new();';
	todo_ok(eval 'defined $spot', " - now real");
}

# rules
# TODO. refer to S05
# L<<S05/"Hypothetical variables" /backtracks past the closure/>>

{
	# - unmatched alternative should bind to undef
	my($num, $alpha);
	my($rx1, $rx2);
	eval '
		$rx1 = rx
			/ [ (\d+)      { let $<num>   := $1 }
			  | (<alpha>+) { let $<alpha> := $2 }
			  ]
			/;
		$rx2 = rx
			/ [ $<num>  := (\d+)
			  | $<alpha>:= (<alpha>+)
			  ]
			/;
	';
	for (<rx1 rx2>) {
		# I want symbolic lookups because I need the rx names for test results.

		eval '"1" ~~ %MY::{$_}';
		todo_ok(defined($num), "{$_}: successful hypothetical");
		ok(!defined($alpha), "{$_}: failed hypothetical");

		eval '"A" ~~ %MY::{$_}';
		ok(!defined($num), "{$_}: failed hypothetical (2nd go)");
		todo_ok(defined($alpha), "{$_}: successful hypothetical (2nd go)");
	}
}

{
	# - binding to hash keys only would leave values undef
	my %matches;
	eval '"a=b\nc=d\n" ~~ / %<matches> := [ (\w) = \N+ ]* /';
	todo_ok(eval '%matches ~~ all(<a b>)', "match keys exist");
	todo_ok(!defined(%matches{"a"}) && !defined(%matches{"b"}),
			"match values don't");
}

{
	# - $1, $2 etc. should all be undef after a failed match
	#   (except for special circumstances)
        "abcde" ~~ rx:perl5/(.)(.)(.)/;
        "abcde" ~~ rx:perl5/(\d)/;
	todo_ok(eval '! grep { defined($_) }, ($1, $2, $3, $4, $5, $6)',
			"all submatches undefined after failed match") or
		diag("match state: " ~ eval '$/');

	# XXX write me: "special circumstances"
}


# subroutines
{
	sub bar ($bar, ?$baz, +$quux) {
		is($bar, "BAR", "defined param"); # sanity

		# L<<S06/"Optional parameters" /Missing optional arguments/>>
		ok(!defined($baz), "unspecified optional param");

		# L<<S06/"Named parameters" /Named parameters are optional/>>
		ok(!defined($quux), "unspecified optional param");
	}

	bar("BAR");

}

# autoloading
# L<S10/Autoloading>

todo_fail("FIXME parsefail (autoload tests)"); # unTODOme
# Currently waiting on
# - packages
# - symtable hash
# - autoloading itself

#{
#	package AutoMechanic {
#		AUTOSCALAR    { \my $_scalar }
#		AUTOARRAY     { \my @_array }
#		AUTOHASH      { \my %_hash }
#		AUTOSUB       { { "code" } }
#		AUTOMETH      { { "code" } }
#
#		AUTOSCALARDEF { %::«'$' ~ $_» = "autoscalardef" }
#		AUTOARRAYDEF  { %::«'@' ~ $_» = "autoarraydef".split(//) }
#		AUTOHASHDEF   { %::«'%' ~ $_» = <autohashdef yes> }
#		AUTOSUBDEF    { %::«'&' ~ $_» = { "autosubdef" } }
#		AUTOMETHDEF   { %::«'&' ~ $_» = { "automethdef" } }
#	}
#
#	is(ref $AutoMechanic::scalar0,    "Scalar", "autload - scalar");
#	is(ref @AutoMechanic::array0,     "Array",  "autload - array");
#	is(ref %AutoMechanic::hash,       "Hash",   "autload - hash");
#	is(ref &AutoMechanic::sub0,       "Code",   "autload - sub");
#	is(ref AutoMechanic.can("meth0"), "Code",   "autload - meth");
#
#	is($AutoMechanic::scalar, "autoscalardef",            "autoloaddef - scalar");
#	is(~@AutoMechanic::ary,   ~("autoarraydef".split(//), "autoloaddef - array");
#	is(~%AutoMechanic::hash,  ~<autohashdef yes>,         "autoloaddef - hash");
#	is(&AutoMechanic::sub.(), "autosubdef",               "autoloaddef - sub");
#	is(AutoMechanic.meth(),   "automethdef",              "autoloaddef - method");
#}