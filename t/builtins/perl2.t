#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Names and Variables /To get a Perlish representation of any data value/>


unless $?PUGS_BACKEND eq "BACKEND_PUGS" {
  skip_rest "eval() not yet implemented in $?PUGS_BACKEND.";
  exit;
}


# Quoting S02 (emphasis added):
#   To get a Perlish representation of any data value, use the .perl method.
#   This will put quotes around strings, square brackets around list values,
#   curlies around hash values, etc., **such that standard Perl could reparse
#   the result**.
sub desc_perl ($obj) {
    "($obj.perl()).perl returned something whose eval()ed stringification is unchanged";
}
sub desc_ref ($obj) {
    "($obj.perl()).perl returned something whose eval()ed .ref is unchanged";
}

{
    # tests 1-6
    for (42, 42/10, 4.2,) -> $obj {
      is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
      is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }
    
    # tests 7,8
    for (sqrt(2)) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj), :todo<bug>;
    }
    
    # tests 9-16
    for (3e5, Inf, -Inf, NaN,) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    for ("a string", "", "\0", "\t", "\n", "\r\n", "\7", '{', '}', "\123",) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    for (?1, ?0, undef,) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    for (rx:Perl5{foo}, rx:Perl5{}, rx:Perl5{^.*$},) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj), :todo<bug>;
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj),  :todo<bug>;
    }

    for (\42, \Inf, \-Inf, \NaN, \"string", \"", \?1, \?0, \undef,) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    # Pairs - XXX - Very Broken - FIXME!
    for ((a => 1),:b(2),) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj), :todo<bug>;
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    for ([],  [ 42 ]     ,  [< a b c>],) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    for ({ a => 42, },  { :a(1), :b(2), :c(3) },) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj), :todo<bug>;
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

#    for ([ 3..42 ], [ 3..Inf ], [ -Inf..Inf ], [ 3..42, 17..Inf, -Inf..5 ],) -> $obj {
#        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
#        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
#    }


    for ({ a => [1,2,3] }, [ [1,2,3] ],) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
    }

    for ({ a => [1,2,3], b => [4,5,6] }) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj);
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj), :todo<bug>;
    }
    for ([ { :a(1) }, { :b(2), :c(3) } ],) -> $obj {
        is ~$obj.perl.eval    , ~$obj    , desc_perl($obj), :todo<bug> ;
        is  $obj.perl.eval.ref,  $obj.ref, desc_ref($obj);
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
