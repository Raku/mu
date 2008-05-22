use v6;

use Test;

=kwid

Splatted parameters shouldn't be rw even if stated as such

=cut

plan 3;

# test splatted parameter for rw ability
# L<S06/"Subroutine traits"/"is rw">

my @test = 1..5;
try {
    my sub should_work ( *@list is rw ) {
        @list[0] = "hi"; 
    }
    should_work(@test);
};

ok(
    !$!,
    "trying to use an 'is rw' splat does work out",
);
is(@test[0], "hi", "@test was unchanged");

try {
    my sub should_work (*@list is rw) { }
};

ok(
    !$!,
    "trying to define an 'is rw' splat works too",
);
