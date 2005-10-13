#!/usr/bin/pugs

use v6;
use Test;

plan 14;

# Since these are all parsing tests, they should use eval to ensure all tests
# can run even if something is broken.  (Unless things are VERY broken.)

if $?PUGS_BACKEND ne "BACKEND_PUGS" {
  skip_rest "PIL2JS and PIL-Run do not support eval() yet.";
  exit;
}

# These tests are for parse-fails:
{
    my sub foo(*@args, *%named) { 1 }

    ok(eval(q/foo;      /), 'call with no args, no parens');
    ok(eval(q/foo();    /), 'call with no args, has parens');
    ok(eval(q/foo.();   /), 'call with no args, has dot and parens');
    ok(eval(q/foo  .(); /), 'call with no args, has dot, spaces, and parens');

    ok(eval(q/foo 1;    /), 'call with one arg, no parens');
    ok(eval(q/foo(1);   /), 'call with one arg, has parens');
    ok(eval(q/foo.(1);  /), 'call with one arg, has dot and parens');
    ok(eval(q/foo  .(1);/), 'call with one arg, has dot, spaces, and parens');
    ok(eval(q/foo'bar'; /), 'call with one arg, has no space and no parens');

    ok(eval(q/foo 1, 2; /), 'call with two args, no parens');
    ok(eval(q/foo(1, 2);/), 'call with two args, has parens');
}


# These tests are for mis-parses:
{
    my sub succ($x) { $x + 1 }

    is(eval(q/succ  (1+2) * 30;/),  91, "parens after space aren't call-parens");
    is(eval(q/succ .(1+2) * 30;/), 120, "parens after space-dot are call-parens", :todo<bug>);
}
{
    my sub first() { "first" }
    my sub second($_) { $_ ~ "second" }
    
    is(eval(q/first.second/), 'firstsecond', '`first.second` means `&second(&first())`');
}
