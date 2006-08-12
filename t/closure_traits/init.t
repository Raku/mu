use v6-alpha;

use Test;

plan 6;

# L<S04/"Closure traits"/INIT "at run time" ASAP>
# INIT {...} blocks in "void" context
{
    my $str;
    is $str, "begin1 begin2 init ", "init blocks run after begin blocks";

    BEGIN { $str ~= "begin1 "; }
    INIT  { $str ~= "init "; }
    BEGIN { $str ~= "begin2 "; }
}

{
    my $str;
    is $str, "check2 check1 init ", "init blocks run after check blocks";

    CHECK { $str ~= "check1 "; }
    INIT  { $str ~= "init "; }
    CHECK { $str ~= "check2 "; }
}

{
    my $str;
    is $str, "begin init1 init2 ", "init blocks run in forward order";

    INIT  { $str ~= "init1 "; }
    BEGIN { $str ~= "begin "; }
    INIT  { $str ~= "init2 "; }
}

# INIT {...} blocks as rvalues
{
    my $str;
    my $handle = { my $retval = INIT { $str ~= 'I' } };

    is $handle(), 'I', 'our CHECK {...} block returned the correct var (1)';
    is $handle(), 'I', 'our CHECK {...} block returned the correct var (2)';
    is $str, 'I', 'our rvalue CHECK {...} block was executed exactly once';
}
