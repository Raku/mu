use v6;

use Test;

plan 11;


# check proper scoping of my in while condition

my $result;
my $x = 0;
is(eval('while my $x = 1 { $result = $x; last }; $result'), 1, 'my in while cond seen from body');
is(eval('while my $x = 1 { last }; $x'), 1, 'my in while cond seen after');

# check proper scoping of my in if condition

is(eval('if my $x = 1 { $x } else { 0 }'), 1, 'my in if cond seen from then');
is(eval('if not my $x = 1 { 0 } else { $x }'), 1, 'my in if cond seen from else');
is(eval('if my $x = 1 { 0 } else { 0 }; $x'), 1, 'my in if cond seen after');

# check proper scoping of my in loop initializer

is(eval('loop (my $x = 1, my $y = 2; $x > 0; $x--) { $result = $x; last }; $result'), 1, '1st my in loop cond seen from body');
is(eval('loop (my $x = 1, my $y = 2; $x > 0; $x--) { $result = $y; last }; $result'), 2, '2nd my in loop cond seen from body');
is(eval('loop (my $x = 1, my $y = 2; $x > 0; $x--) { last }; $x'), 1, '1st my in loop cond seen after');
is(eval('loop (my $x = 1, my $y = 2; $x > 0; $x--) { last }; $y'), 2, '2nd my in loop cond seen after');

# check that can declaring lexical twice is noop
{
    my $f;
    $f = 5;
    my $f;
    is($f, 5, "two lexicals declared in scope is noop");
}

my $x = 42;
{
    my $x = $x;
    is( $x, undef, 'my $x = $x; can not see the value of the outer $x');
}
