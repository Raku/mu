#!/usr/bin/pugs

use v6;
use Test;

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
	ok(defined(%hash<bar>), "hash subscript");
	ok(!defined(%hash<bargho>), "non-existent hash subscript");

	undef %hash<bar>;
	ok(!defined(%hash<bar>), "undef hash subscript");

	%hash<bar> = "baz";
	%hash.delete("bar");
	ok(!defined(%hash<bar>), "delete hash subscript");

	ok(defined(@ary), "aggregate array defined");
	ok(defined(%hash), "aggregate hash defined");

	undef(@ary);
    ok(!defined(@ary), "undef array",:todo<bug>);

	undef(%hash);
    ok(!defined(%hash), "undef hash",:todo<bug>);

	@ary = (1);
	ok(defined(@ary), "define array again");
	%hash = (1,1);
	ok(defined(%hash), "define hash again");
}

{
	sub a_sub { "møøse" }

	ok(defined(&a_sub), "defined sub");
	eval_ok('defined(%«$?PACKAGE\::»<&a_sub>)', "defined sub (symbol table)", :todo<parsefail>);

	eval_ok('!defined(&a_subwoofer)', "undefined sub",:todo<feature>);
	eval_ok('!defined(%«$?PACKAGE\::»<&a_subwoofer>)', "undefined sub (symbol table)", :todo<feature>);
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
(undef, undef, $interesting) = (1,2,3);
is($interesting, 3, "Undef on LHS of list assignment");

(undef, $interesting, undef) = (1,2,3);
is($interesting, 2, "Undef on LHS of list assignment");

($interesting, undef, undef) = (1,2,3);
is($interesting, 1, "Undef on LHS of list assignment");

sub two_elements() { (1,2) };
(undef,$interesting) = two_elements();
is($interesting, 2, "Undef on LHS of function assignment");

($interesting, undef) = two_elements();
is($interesting, 1, "Undef on LHS of function assignment");

=kwid

Perl6-specific tests

=cut

{
	# aggregate references

	my @ary = (<a b c d e>);
	my $ary_r = @ary; # ref
	isa_ok($ary_r, "Array");
	ok(defined($ary_r), "array reference");

	undef @ary;
	ok(!+$ary_r, "undef array referent");

	is(+$ary_r, 0, "dangling array reference");

	my %hash = (1, 2, 3, 4);
	my $hash_r = %hash;
	isa_ok($hash_r, "Hash");
	ok(defined($hash_r), "hash reference");
	undef %hash;
	ok(defined($hash_r), "undef hash referent:");
	is(+$hash_r.keys, 0, "dangling hash reference");
}

{
	# types
	# TODO: waiting on my Dog $spot;

	eval 'my Array $an_ary';
	ok(eval '!defined($an_ary)', "my Array", :todo);
	ok(eval '!defined($an_ary.0)', "my Array subscript - undef", :todo);
	eval '$an_ary.push("blergh")';
	ok(eval 'defined($an_ary.pop)', "push", :todo);
	ok(eval '!defined($an_ary.pop)', "comes to shove", :todo);

	eval 'my Hash $a_hash';
	ok(eval '!defined($a_hash)', "my Hash", :todo);
	ok(eval '!defined($a_hash<blergh>)', "my Hash subscript - undef", :todo);
	ok(eval '!defined($a_hash<blergh>)', "my Hash subscript - undef, even after autovivification", :todo);
	eval '$a_hash<blergh> = 1';
	ok(eval 'defined($a_hash<blergh>.delete)', "delete", :todo);
	ok(eval '!defined($a_hash<blergh>.delete)', " - once only", :todo);

	eval '
		class Dog {};
		my Dog $spot;
	';

	ok(eval '!defined $spot', "Unelaborated mutt", :todo);
	eval '$spot .= .new();';
	ok(eval 'defined $spot', " - now real", :todo);
}

# rules
# TODO. refer to S05
# L<S05/"Hypothetical variables" /backtracks past the closure/>

{
	# - unmatched alternative should bind to undef
	my($num, $alpha);
	my($rx1, $rx2);
	eval '
		$rx1 = rx
			/ [ (\d+)      { let $<num>   := $0 }
			  | (<alpha>+) { let $<alpha> := $1 }
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
		ok(defined($num), "{$_}: successful hypothetical", :todo);
		ok(!defined($alpha), "{$_}: failed hypothetical");

		eval '"A" ~~ %MY::{$_}';
		ok(!defined($num), "{$_}: failed hypothetical (2nd go)");
		ok(defined($alpha), "{$_}: successful hypothetical (2nd go)", :todo);
	}
}


if(eval('!("a" ~~ /a/)')) {
  skip 2, "skipped tests - rules support appears to be missing";
}
else {
	# - binding to hash keys only would leave values undef
	eval '"a=b\nc=d\n" ~~ / $<matches> := [ (\w) = \N+ ]* /';
	ok(eval '$<matches> ~~ all(<a b>)', "match keys exist", :todo);

	ok(!defined($<matches><a>) && !defined($<matches><b>), "match values don't");
}

{
	# - $0, $1 etc. should all be undef after a failed match
	#   (except for special circumstances)
        "abcde" ~~ rx:perl5/(.)(.)(.)/;
        "abcde" ~~ rx:perl5/(\d)/;
	ok((!grep { defined($_) } ($0, $1, $2, $3, $4, $5)),
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

fail("FIXME (autoload tests)", :todo<parsefail>);
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
#		AUTOSCALARDEF { %::«{'$' ~ $_}» = "autoscalardef" }
#		AUTOARRAYDEF  { %::«{'@' ~ $_}» = "autoarraydef".split("") }
#		AUTOHASHDEF   { %::«{'%' ~ $_}» = <autohashdef yes> }
#		AUTOSUBDEF    { %::«{'&' ~ $_}» = { "autosubdef" } }
#		AUTOMETHDEF   { %::«{'&' ~ $_}» = { "automethdef" } }
#	}
#
#	is(ref $AutoMechanic::scalar0,    "Scalar", "autload - scalar");
#	is(ref @AutoMechanic::array0,     "Array",  "autload - array");
#	is(ref %AutoMechanic::hash,       "Hash",   "autload - hash");
#	is(ref &AutoMechanic::sub0,       "Code",   "autload - sub");
#	is(ref AutoMechanic.can("meth0"), "Code",   "autload - meth");
#
#	is($AutoMechanic::scalar, "autoscalardef",            "autoloaddef - scalar");
#	is(~@AutoMechanic::ary,   ~("autoarraydef".split(""), "autoloaddef - array");
#	is(~%AutoMechanic::hash,  ~<autohashdef yes>,         "autoloaddef - hash");
#	is(&AutoMechanic::sub.(), "autosubdef",               "autoloaddef - sub");
#	is(AutoMechanic.meth(),   "automethdef",              "autoloaddef - method");
#}

# Extra tests added due to apparent bugs
eval_is('undef + 1', undef, 'undef + 1', :todo<bug>);
eval_is('1 + undef', undef, '1 + undef', :todo<bug>);
eval_is('undef * 2', undef, 'undef * 2');
eval_is('2 * undef', undef, '2 * undef', :todo<bug>);
eval_is('undef xx 2', undef, 'undef xx 2', :todo<bug>);
eval_is('undef * undef', undef, 'undef * undef');
