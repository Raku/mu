use v6-alpha;

use Test;

plan 13;

# L<S04/Closure traits/FIRST "runs seperately for each clone">

{
    my $var;
    my $was_in_first = 0;
    my $sub = { FIRST { $was_in_first++; $var = rand } };

    is $var, undef, 'FIRST {...} not run yet';

    $sub();
    is $was_in_first, 1, 'our FIRST {} block was invoked';
    my $orig_var = $var;

    $sub();
    is $was_in_first, 1, 'our FIRST {} block was invoked only once...';
    is $var, $orig_var, "...and our var wasn't changed";
}

{
    my $sub = {
        my $str ~= 'o';
        FIRST { $str ~= 'I' };
        FIRST { $str ~= 'i' };
        ":$str";
    };

    is $sub(), ':Iio', 'FIRST {} block set $str to 3';
    is $sub(), ':o', 'FIRST {} wasn\'t invoked again (1-1)';
    is $sub(), ':o', 'FIRST {} wasn\'t invoked again (1-2)';
}

{
    my $was_in_first;
    my $sub = {
      my $var = FIRST { $was_in_first++; 23 };
      $var //= 42;
      $var;
    };

    is $was_in_first, undef, 'FIRST {} not run yet';
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
