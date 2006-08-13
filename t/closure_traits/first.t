use v6-alpha;

use Test;

plan 21;

# L<S04/Closure traits/FIRST "runs seperately for each clone">

{
    my $var;
    my $was_in_first = 0;
    my $sub = { FIRST { $was_in_first++; $var += 100 } };

    is $var, undef, 'FIRST {...} not run yet';

    $sub();
    is $var, 100, 'FIRST {} has executed';
    is $was_in_first, 1, 'our FIRST {} block was invoked';

    $sub();
    is $was_in_first, 1, 'our FIRST {} block was invoked only once...';
    is $var, 100, "...and our var wasn't changed";
}

# IRC note
# ----------------------------------------------------
# <TimToady>  when you say "state $x = func()", func()
#             has to have access to everything that
#             happened right up until that moment the
#             = is executed, which is the same moment
#             it would have been exectued for a my.
#             the difference is it only does it once.
{
    my $sub = {
        my $str ~= 'o';
        FIRST { $str ~= 'I' };
        FIRST { $str ~= 'i' };
        ":$str";
    };

    is $sub(), ':oIi', 'FIRST {} block set $str to 3';
    is $sub(), ':o', 'FIRST {} wasn\'t invoked again (1-1)';
    is $sub(), ':o', 'FIRST {} wasn\'t invoked again (1-2)';
}

# IRC note
# --------------------------------------------------------
# <TimToady> the my example would calculate a FIRST value,
#            and then every time
# <TimToady> through reinit $x to that value.
{
    my $was_in_first;
    my $sub = {
      my $var = FIRST { $was_in_first++; 23 };
      $var //= 42;
      $var;
    };

    is $was_in_first, undef, 'FIRST {} has not run yet';
    is $sub(), 23, 'FIRST {} block set our variable (2)';
    is $sub(), 23, 'the returned value of FIRST {} still there';
    is $was_in_first, 1, 'our FIRST {} block was invoked exactly once';
}

# Test that FIRST {} blocks are executed only once even if they return undef
# (the first implementation ran than twice instead).
{
    my $was_in_first;
    my $sub = { FIRST { $was_in_first++; undef } };

    is $sub(), undef, 'FIRST {} returned undef';
    $sub();
    $sub();
    is $was_in_first, 1,
        'our FIRST { ...; undef } block was invoked exactly once';
}

# IRC note:
# ---------------------------------------------------------------
# <TimToady1> FIRST runs later when you first try to use a block.
# <TimToady1> a FIRST will run multiple times in a block that is 
#             cloned as a closure.
# <TimToady1> INIT only ever runs once.
{
    my $str ~= 'o';
    {
        FIRST { $str ~= 'i' }
    }
    is $str, 'oi', 'FIRST {} runs when we first try to use a block';
}

{
    my $var;
    my $sub = { FIRST { $var++ } };
    is $var, undef, 'FIRST {} has not run yet';
    $sub();
    is $var, 1, '$sub\'s FIRST {} only runs once';

    my $sub2 = { $sub() };
    is $var, 1, 'FIRST {} has not run for the second time yet';
    $sub2();
    is $var, 2, '$sub2\'s FIRST {} only runs once';

    my $sub3 = { $sub2() };
    is $var, 2, 'FIRST {} has not run for the third time yet';
    $sub3();
    is $var, 3, '$sub3\'s FIRST {} only runs once';
}
