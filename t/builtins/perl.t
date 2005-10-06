#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Names and Variables /To get a Perlish representation of any data value/>

my @tests = (
    # Basic scalar values
    42, 3e5, Inf, -Inf, NaN,
    "a string", "", "\0", "\t", "\n", "\r\n",
    ?1, ?0,
    undef,

    # References to scalars
    \42, \Inf, \"string", \"", \?1, \?0, \undef,

    # Pairs - XXX - Very Broken - FIXME!
#    (a => 1),
#    :b(2),

    # References to aggregates
    [< a b c>],
    { :a(1), :b(2), :c(3) },

    # Nested things
    { a => [1,2,3], b => [4,5,6] },
    [ { :a(1) }, { :b(2), :c(3) } ],
);

plan 7 + 2*@tests;
force_todo 4, 7..8, 45..49, 52, 54, 56;

unless $?PUGS_BACKEND eq "BACKEND_PUGS" {
  skip_rest "eval() not yet implemented in $?PUGS_BACKEND.";
  exit;
}


# Quoting S02 (emphasis added):
#   To get a Perlish representation of any data value, use the .perl method.
#   This will put quotes around strings, square brackets around list values,
#   curlies around hash values, etc., **such that standard Perl could reparse
#   the result**.
{
    for @tests -> $obj {
        is ~$obj.perl.eval, ~$obj,
            ".perl returned something whose eval()ed stringification is unchanged";
        is $obj.perl.eval.ref, $obj.ref,
            ".perl returned something whose eval()ed .ref is unchanged";
    }
}

# Recursive data structures
{
    my $foo = [ 42 ]; $foo[1] = $foo;
    is $foo[1][1][1][0], 42, "basic recursive arrayref";

    # XXX hangs
    fail "skipping hanging test";
    #is ~$foo.perl.eval, ~$foo,
    #    ".perl worked correctly on a recursive arrayref";
}

{
    my $foo = { a => 42 }; $foo<b> = $foo;
    is $foo<b><b><b><a>, 42, "basic recursive hashref";

    # XXX hangs
    fail "skipping hanging test";
    #is ~$foo.perl.eval, ~$foo,
    #    ".perl worked correctly on a recursive hashref";
}

{
    my $foo = [ 42 ];
    my $bar = { a => 23 };
    $foo[1] = $bar;
    $bar<b> = $foo;

    is $foo[1]<b>[1]<b>[0], 42, "mixed arrayref/hashref recursive structure";

    # XXX hangs
    fail "skipping hanging test";
    #is ~$foo.perl.eval, ~$foo,
    #    ".perl worked correctly on a mixed arrayref/hashref recursive structure";
}

{
    # test a bug reported by Chewie[] - apparently this is from S03
    is((("f","oo","bar").keys).perl, "(0, 1, 2)", ".perl on a .keys list");
}
