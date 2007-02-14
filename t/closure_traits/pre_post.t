use v6-alpha;

use Test;
# Test of PRE and POST traits
#
# L<S04/Closure traits>
#
# I don't get this one working:
## L<S06/Properties and traits/Subroutine traits>

# TODO: Test PRE and POST with inheritance

plan 4;

my $foo = '
sub foo(int $i) {
    PRE {
        $i < 5;
    }
    return 1;
}
';

sub bar(int $i) {
    return 1;
    POST {
        $i < 5;
    }
}

ok eval($foo ~ 'foo(2)'), 'sub with PRE compiles and runs';
ok eval(bar(3)), 'sub with POST compiles';

try {
    eval($foo ~ 'foo(10)');
}

ok defined($!), 'Violated PRE fails OK';

try {
    bar(10);
}
ok defined($!), 'violated POST fails OK';

