#!/usr/bin/pugs

use v6;
require Test;

=kwid

Basic "undef" tests

=cut

plan 24;

=kwid

This test file contains two sections: a port of the perl5 undef.t tests,
and perl6-specific tests.

First the perl5 parts.

=cut

my $a;
is($a, undef, "uninitialized lexicals are undef");
todo_is(eval '$y', undef, "uninitialized globals are undef"); # turn off strict?

$a += 1; # should not emit a warning. how to test that?
ok(defined($a), "initialized var is defined");

undef $a;
ok(!defined($a), "undef($a) does");

$a = "hi";
ok(defined($a), "string");

my $b;
$a = $b;
ok(!defined($a), "assigning another undef lexical");

##???todo_is(eval '$a = $c; !defined($a)'); # todo: the same with a global

my @ary = "arg1";
$a = @ary.pop;
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
ok(!defined(%hash{"bargho"}), "non-existent hash subscript") or
	diag("expected undef; got { %hash{'bagho'} }");
#undef %hash{"bar"}; XXX: FIXME
##delete %hash{"bar"};
#ok(!defined(%hash{"bar"}), "undef hash subscript");

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

# ???
#sub a_sub { pass "sub"; 1 }
#a_sub || fail "a_sub";

# TODO: defined a_sub

# TODO: find a read-only value to try and assign to, since we don't
# have rules right now to play around with (the p5 version used $1)
#eval { "constant" = "something else"; };
#is($!, "Modification of a read", "readonly write yields exception");

# skipped tests for tied things

# skipped test for attempt to undef a bareword -- no barewords here.

# TODO: p5 "bugid 3096
# undefing a hash may free objects with destructors that then try to
# modify the hash. To them, the hash should appear empty."


=kwid

Perl6-specific tests

=cut

# aggregate references

@ary = (<a b c d e>);
my $ary_r = @ary; # ref
is(ref($ary_r), "Array", "taking a ref");
ok(defined($ary_r), "array reference");
undef @ary;
ok(defined($ary_r), "undef array referent");
todo_is(eval '$ary_r.elems', 0, "dangling array reference") or diag $ary_r;

%hash = (1, 2, 3, 4);
my $hash_r = %hash;
is(ref($hash_r), "Hash", "taking a ref");
ok(defined($hash_r), "hash reference");
undef %hash;
ok(defined($hash_r), "undef hash referent");
todo_is(eval '$hash_r.keys.elems', 0,"dangling hash reference") or diag $hash_r;


# rules
# TODO: write me. refer to S05
# - unmatched alternative should bind to undef
# - binding to hash keys only would leave values undef
# - $1, $2 etc. should all be undef after a failed match
#   (except for special circumstances)

# subroutines
# TODO: write me. refer to S06
# - a sub with optional args and named parameters which don't have
#   defaults specified, when called without values will yield undef

# autoloading
# TODO: write me, refer to S09
# - autoloading


