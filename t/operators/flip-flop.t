#!/usr/bin/pugs

use v6;
use Test;

plan 22;

# L<S03/"Operator renaming" /flipflop operator is now done with/>

# XXX tests for fff

sub take (Int $n, Code &f) { (1..$n).map:{ try { f() } } }
sub always_false { 0 }
sub always_true  { 1 }

# Basic ff
{
   eval_ok '1 ff 1', 'flip-flop operator implemented', :todo<feature>;

}

{
    my @result = eval('take 5, { ?(always_false() ff always_false()) }');
    is ~@result, "    ", "always_false() ff always_false()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_false() ff always_true()) }');
    is ~@result, "    ", "always_false() ff always_true()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_true() ff always_true()) }');
    ok all(@result), "always_true() ff always_true()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_true() ff always_false()) }');
    is ~@result, "1 2 3 4 5", "always_true() ff always_false()", :todo<feature>;
}

# Basic ^ff
{
    my @result = eval('take 5, { ?(always_false() ^ff always_false()) }');
    is ~@result, "    ", "always_false() ^ff always_false()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_false() ^ff always_true()) }');
    is ~@result, "    ", "always_false() ^ff always_true()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_true() ^ff always_true()) }') || 1;
    my $first  = shift @result;

    ok !$first && all(@result), "always_true() ^ff always_true()",:todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_true() ^ff always_false()) }');
    is ~@result, " 2 3 4 5", "always_true() ^ff always_false()", :todo<feature>;
}

# Basic ff^
{
    my @result = eval('take 5, { ?(always_false() ff^ always_false()) }');
    is ~@result, "    ", "always_false() ff^ always_false()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_false() ff^ always_true()) }');
    is ~@result, "    ", "always_false() ff^ always_true()", :todo<feature>;
}

{
    my @result = eval('take 5, { ?(always_true() ff^ always_true()) }');

    # XXX what should the result be?
}

{
    my @result = eval('take 5, { ?(always_true() ff^ always_false()) }');
    is ~@result, "1 2 3 4 5", "always_true() ff^ always_false()", :todo<feature>;
}

# RHS not evaluated when in "false" state (perldoc perlop, /flip-flop)
{
    { my $bug; eval_ok '0 ff {$bug=2};$bug ||=1',:todo<feature>; ok ($bug == 1), "RHS not evaluated in \"false\" state (ff)", :todo<feature>; }
    { my $bug; eval_ok '0 ^ff {$bug=2};$bug ||=1',:todo<feature>; ok ($bug == 1), "RHS not evaluated in \"false\" state (^ff)", :todo<feature>; }
    { my $bug; eval_ok '0 ff^ {$bug=2};$bug ||=1',:todo<feature>; ok ($bug == 1), "RHS not evaluated in \"false\" state (ff^)", :todo<feature>; }
}

# LHS not evaluated when in "true" state (perldoc perlop, /flip-flop)
{
    my sub true_then_die {
        state $invoked;
        unless $invoked++ {
            "true";
        } else {
            die;
        }
    }
    eval_ok 'true_then_die()  ff  always_false();true_then_die() ff always_false()',
        "LHS not evaluated in \"true\" state (ff)", :todo<feature>;
    eval_ok 'true_then_die() ^ff  always_false();true_then_die() ^ff always_false()',
        "LHS not evaluated in \"true\" state (^ff)", :todo<feature>;
    eval_ok 'true_then_die()  ff^ always_false();true_the_die() ff^ always_false()',
        "LHS not evaluated in \"true\" state (ff^)", :todo<feature>;
}

# See thread "till (the flipflop operator, formerly ..)" on p6l started by Ingo
# Blechschmidt, especially Larry's reply:
# http://www.nntp.perl.org/group/perl.perl6.language/24098
{
    eval_ok 'my sub foo ($x) { try { $x ff 0 } }; if foo(0) || !foo(1) || !foo(0) { die }',
    	"all sub invocations share the same ff-state", :todo<feature>;
}
