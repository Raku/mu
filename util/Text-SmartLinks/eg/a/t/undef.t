use v6;

use Test;

=begin pod

`undef` and `undefine` tests

This test file contains two sections: a port of the perl5 `undef.t` tests, and
perl6-specific tests.

=end pod

# Note: See thread "Undef issues" by Adrian Taylor on p6l
# L<http://groups.google.com/groups?threadm=20050601002444.GB32060@wall.org>

plan 75;

our $GLOBAL;

# L<S29/Scalar/"=item undef">

is(undef, undef, "undef is equal to undef");
ok(!defined(undef), "undef is not defined");

{
    my $a;
    ok(!defined($a), "uninitialized lexicals are undef");
}


# L<S29/Scalar/"=item undefine">
{
    my @ary = "arg1";
    my $a = @ary.pop;
    ok(defined($a), "pop from array");
    $a = @ary.pop;
    ok(!defined($a), "pop from empty array");
}

#?rakudo skip 'access to &your_sub'
{
    sub a_sub { "møøse" }

    ok(defined(&a_sub), "defined sub");
}

# rules
# TODO. refer to S05
# L<S05/Match objects/"they will all be undefined" closure
#                                 "let keyword">

# - unmatched alternative should bind to undef
#?rakudo skip 'null PMC access in type()'
#?DOES 10
{
    my ($num, $alpha);
    my ($rx1, $rx2);
    eval '
        $rx1 = rx
        ';
        ok(defined($num), '{$_}: successful hypothetical');
}

# subroutines
{
    sub bar ($bar, $baz?, :$quux) {
        is($bar, "BAR", "defined param"); # sanity

        # L<<S06/Optional parameters/Missing optional arguments>>
        ok(!defined($baz), "unspecified optional param");

        # L<S06/Named parameters/Named parameters are optional>
        ok(!defined($quux), "unspecified optional param");
    }

    bar("BAR");

}

# autoloading
# L<S10/Autoloading>

#?pugs skip 'parsefail'
#?rakudo skip 'parsefail'
flunk('FIXME: parsefail');

# Extra tests added due to apparent bugs
is((undef) + 1, 1, 'undef + 1');
is(1 + (undef), 1, '1 + undef');

# L<http://colabti.de/irclogger/irclogger_log/perl6?date=2006-09-12,Tue&sel=145#l186>
# See log above.  From IRC, TimToady says that both of these
# should be false.  (At time of writing, @(undef,) is true.)
is ?(@(undef,)), Bool::False, '?(@(undef,)) is false';


# L<S19/Something else>
is ?(list(undef,)), Bool::False, '?(@(undef,)) is false';

