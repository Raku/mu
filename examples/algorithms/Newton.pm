package Newton;

use v6;

sub update_guess(Num $guess is rw, Num $target, Code $f, Code $fprime) {
    $guess += ($target - $f($guess)) / $fprime($guess);
}

sub approxfprime(Code $f, Num $x) {
    my Num $delta = 0.1;
    return ($f($x + $delta) - $f($x - $delta))/(2 * $delta);
}

sub newton(
    Num  $target,
    Code $f,
    Code +$fprime         = &approxfprime.assuming( f => $f ),
    Num  +$epsilon        = 0.0005,
    Num  +$max_iterations = 500,
    Bool +$verbose        = 0
) returns Num is export {
    my Num $guess  = $target / 2;
    my Int $count  = 1;

    while (abs($f($guess) - $target) > $epsilon) {

        if ($count > $max_iterations) {
            die "excessive looping";
        }

        update_guess($guess, $target, $f, $fprime);

        say "{$count++}: $guess" if $verbose;
    }
    return $guess;
}

=head1 NAME

Newton - performs one dimensional Newton's method

=head1 SYNOPSIS

    #!/usr/bin/pugs
    use v6;

    use Newton;

    sub f(Num $x) { return $x ** 3; }

    say "The cube root of 7 is: {newton(7, &f, $verbose => 1)}";

=head1 DESCRIPTION

Newton's method is a simple method for finding roots, like square roots or
cube roots (as in the example above).  Read a Calculus textbook for details.
This implementation is meant to show the power of Perl 6 defaults and
Currying in a real context.

The newton function must receive at least:

    y - target value for y
    f - function of one variable

It looks for an x such that f(x) is y.

You may optionally supply:

    epsilon - tolerance (how accurate the answer must be)
    fprime  - an exact first derivative of f (which Newton's method
              uses to update its guess at each iteration)
    verbose - a boolean which will turn on per iteration printing
    
If you omit the fprime, a second order centered difference is provided
as a curried default.

All of the optional parameters should be supplied by name.

Note that the above code is not robust.  (If you need a real implementation
consult a book, like Numerical Recipies.)  In particular, it suffers from
convergence problems for some functions, which all Newton methods do.  All
it does in such cases is die with a complaint.  Real solvers usually do
a bit of algorithm switching.  When their fast method fails, they fall back
on something more likely to work.

Further, the above method always uses a starting guess which is half
the target.  This does not facilitate recovering all the roots of a
function (not that Newton's method is ever really good at that).

=head2 The Defaults

Note that Perl 6 allows us to provide defaults for optional parameters.
This includes providing a default fprime in the example.  For fprime, the
example goes on step further and Curries approxfprime with the user supplied
f.  Such things are possible in Perl 5, but simply saying .assuming is
nicer.

=cut
