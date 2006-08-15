use v6-alpha;

use Test;

plan 22;

# L<S04/Closure traits/START "runs seperately for each clone">

{
    my $var;
    my $was_in_start = 0;
    my $sub = { START { $was_in_start++; $var += 100 } };

    is $var, undef, 'START {...} not run yet';

    $sub();
    is $var, 100, 'START {} has executed';
    is $was_in_start, 1, 'our START {} block was invoked';

    $sub();
    is $was_in_start, 1, 'our START {} block was invoked only once...';
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
        START { $str ~= 'i' }
    }
    is $str, 'oi', 'START {} runs when we first try to use a block';
}

# Execute the tests twice to make sure that START binds to
# the lexical scope, not the lexical position.
for <first second> {
    my $sub = {
        my $str = 'o';
        START { $str ~= 'I' };
        START { $str ~= 'i' };
        ":$str";
    };
	
    is $sub(), ':oIi', "START block set \$str to 3     ($_ time)";
    is $sub(), ':o', "START wasn't invoked again (1-1) ($_ time)";
    is $sub(), ':o', "START wasn't invoked again (1-2) ($_ time)";
}

# Some behavior occurs where START does not close over the correct
# pad when closures are cloned

my $ran;
for <first second> {
    my $str = 'bana';
    $ran = 0;
    my $sub = {
        START { $ran++; $str ~= 'na' };
    };

    $sub(); $sub();
    is $ran, 1, "START block ran exactly once ($_ time)";
    is $str, 'banana', "START block modified the correct variable ($_ time)";
}

# IRC note
# --------------------------------------------------------
# <TimToady> the my example would calculate a FIRST value,
#            and then every time
# <TimToady> through reinit $x to that value.
{
    my $was_in_start;
    my $sub = {
      my $var = START { $was_in_start++; 23 };
      $var //= 42;
      $var;
    };

    is $was_in_start, undef, 'START {} has not run yet';
    is $sub(), 23, 'START {} block set our variable (2)';
    is $sub(), 23, 'the returned value of START {} still there';
    is $was_in_start, 1, 'our START {} block was invoked exactly once';
}

# Test that START {} blocks are executed only once even if they return undef
# (the first implementation ran them twice instead).
{
    my $was_in_start;
    my $sub = { START { $was_in_start++; undef } };

    is $sub(), undef, 'START {} returned undef';
    $sub();
    $sub();
    is $was_in_start, 1,
        'our START { ...; undef } block was invoked exactly once';
}
