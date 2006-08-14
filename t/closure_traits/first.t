use v6-alpha;

use Test;

plan 22;

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
# ...
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

# Execute the tests twice to make sure that FIRST binds to
# the lexical scope, not the lexical position.
for <first second> {
    my $sub = {
        my $str = 'o';
        FIRST { $str ~= 'I' };
        FIRST { $str ~= 'i' };
        ":$str";
    };
	
    is $sub(), ':oIi', "FIRST block set \$str to 3     ($_ time)";
    is $sub(), ':o', "FIRST wasn't invoked again (1-1) ($_ time)";
    is $sub(), ':o', "FIRST wasn't invoked again (1-2) ($_ time)";
}

# Some behavior occurs where FIRST does not close over the correct
# pad when closures are cloned

my $ran;
for <first second> {
    my $str = 'bana';
    $ran = 0;
    my $sub = {
        FIRST { $ran++; $str ~= 'na' };
    };

    $sub(); $sub();
    is $ran, 1, "FIRST block ran exactly once ($_ time)";
    is $str, 'banana', "FIRST block modified the correct variable ($_ time)";
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
