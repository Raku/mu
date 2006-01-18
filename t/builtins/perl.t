#!/usr/bin/pugs

use v6;
use Test;

# L<S02/"Names and Variables" /To get a Perlish representation of any data value/>

my @tests = (
    # Basic scalar values
    42, 42/10, 4.2, sqrt(2), 3e5, Inf, -Inf, NaN,
    "a string", "", "\0", "\t", "\n", "\r\n", "\7", '{', '}', "\123", '$a @string %with &sigils()',
    ?1, ?0,
    undef,
    rx:Perl5{foo}, rx:Perl5{}, rx:Perl5{^.*$},

    # References to scalars
    \42, \Inf, \-Inf, \NaN, \"string", \"", \?1, \?0, \undef,

    # Pairs - XXX - Very Broken - FIXME!
    (a => 1),
    :b(2),

    # References to aggregates
    [],      # empty array
    [ 42 ],  # only one elem
    [< a b c>],
    {},           # empty hash
    { a => 42 },  # only one elem
    { :a(1), :b(2), :c(3) },

    # Infinite/lazy arrays, commented because they take infram and inftime in
    # current Pugs
    # [ 3..42 ],
    # [ 3..Inf ],
    # [ -Inf..Inf ],
    # [ 3..42, 17..Inf, -Inf..5 ],

    # Nested things
    { a => [1,2,3] },  # only one elem
    [      [1,2,3] ],  # only one elem
    { a => [1,2,3], b => [4,5,6] },
    [ { :a(1) }, { :b(2), :c(3) } ],
);

plan 7 + 2*@tests;
force_todo 8, 45..50, 89, 94, 96;

unless $?PUGS_BACKEND eq "BACKEND_PUGS" {
  skip_rest "eval() not yet implemented in $?PUGS_BACKEND.";
  exit;
}


# L<S02/"Names and Variables" /to get a Perlish representation/>
# Quoting S02 (emphasis added):
#   To get a Perlish representation of any data value, use the .perl method.
#   This will put quotes around strings, square brackets around list values,
#   curlies around hash values, etc., **such that standard Perl could reparse
#   the result**.
{
    for @tests -> $obj {
        is ~$obj.perl.eval, ~$obj,
            "($obj.perl()).perl returned something whose eval()ed stringification is unchanged";
        is $obj.perl.eval.ref, $obj.ref,
            "($obj.perl()).perl returned something whose eval()ed .ref is unchanged";
    }
}

# Recursive data structures
{
    my $foo = [ 42 ]; $foo[1] = $foo;
    is $foo[1][1][1][0], 42, "basic recursive arrayref";

    # XXX hangs
    flunk "skipping hanging test", :todo<feature>;
    #is ~$foo.perl.eval, ~$foo,
    #    ".perl worked correctly on a recursive arrayref";
}

{
    my $foo = { a => 42 }; $foo<b> = $foo;
    is $foo<b><b><b><a>, 42, "basic recursive hashref";

    # XXX hangs
    flunk "skipping hanging test", :todo<feature>;
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
    flunk "skipping hanging test", :todo<feature>;
    #is ~$foo.perl.eval, ~$foo,
    #    ".perl worked correctly on a mixed arrayref/hashref recursive structure";
}

{
    # test a bug reported by Chewie[] - apparently this is from S03
    is((("f","oo","bar").keys).perl, "(0, 1, 2)", ".perl on a .keys list");
}
